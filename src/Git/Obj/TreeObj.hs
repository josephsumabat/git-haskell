{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Git.Obj.TreeObj (
    TreeListObj(..)
  , FileObjType(..)
  , PathEntry(..)
  , pathFormat
  , treeFormat
  , treeToText
  , expandTree
  , objToFileType
) where

import           Control.Applicative         (some)
import           Data.Void                  (Void)
import           Data.List                  (intercalate)

import qualified Data.Text                  as T   (Text, concat, intercalate, length, pack, unpack)
import qualified Data.Text.Encoding         as T   (encodeUtf8, decodeUtf8)
import qualified Text.Megaparsec            as MP  (Parsec, parse, takeP, eof,)
import qualified Text.Megaparsec.Byte       as MP  ( char
                                                   , printChar
                                                   , space1)
import qualified Text.Megaparsec.Byte.Lexer as MP  (decimal)
import qualified Data.ByteString            as BS  (ByteString, concat, pack)
import qualified Data.ByteString.Base16     as B16 (encode, decode)
import qualified Data.ByteString.Char8      as C8  (pack)
import           Git.DefinedExceptions          (ObjException (..)
                                                , rightToMaybe
                                                , maybeExceptionHelper)
import           Git.Obj.BlobObj                (BlobObj (..))
import           Git.Obj.RawObj                 (ObjHash, ObjType (..),
                                                 GitObj (..), RawObj (..)
                                                , objTypeToText)
import           Git.Obj.Db                     (readObj)

newtype TreeListObj =
  TreeListObj {
  files :: [PathEntry]
} deriving (Show)

data PathEntry = PathEntry
  {
    pFileMode :: Int
  , pFileName :: FileName
  , pHash     :: ObjHash
  } deriving (Eq, Show)

data FileObjType = BlobFileType | TreeFileType deriving (Eq, Show)
data TreeObj = TreeObjLeaf PathEntry BlobObj 
             | TreeObjBranch PathEntry [ TreeObj ] 
             | TreeObjRoot [ TreeObj ]
             deriving (Show)

type FileName   = T.Text
type TreeDepth = Integer

type TreeNodeInfo = ( PathEntry, ObjType, FilePath, TreeDepth)

type TreeParser = MP.Parsec Void BS.ByteString TreeListObj
type PathParser = MP.Parsec Void BS.ByteString PathEntry

instance GitObj TreeListObj where
  fromRawMaybe (RawObj Tree _ r) = rightToMaybe (MP.parse treeFormat "" r)
  fromRawMaybe _  = Nothing
  gObjType _ = Tree
  serializeObj (TreeListObj l) = BS.concat $ serializePathEntry <$> l

objToMaybeFileType :: ObjType -> Maybe FileObjType
objToMaybeFileType Blob = Just BlobFileType
objToMaybeFileType Tree = Just TreeFileType
objToMaybeFileType _    = Nothing

objToFileType :: ObjType -> FileObjType
objToFileType =
  maybeExceptionHelper objToMaybeFileType UnexpectedObjectException

expandTree :: TreeListObj -> IO TreeObj
expandTree t = TreeObjRoot <$> (traverse expandPath $ files t)
  where
  expandTree' :: PathEntry -> TreeListObj -> IO TreeObj
  expandTree' p l = (TreeObjBranch p) <$> (traverse expandPath $ files l)
  expandPath :: PathEntry -> IO TreeObj
  expandPath path = let h = pHash path in
    (readObj h) >>= \rawObj ->
      case (objToFileType $ objType rawObj) of
          BlobFileType -> TreeObjLeaf path <$> fromRaw <$> readObj h
          TreeFileType -> (expandTree' path) =<< (fromRaw <$> (readObj h))


preOrderTreeFlat :: TreeObj -> [TreeNodeInfo]
preOrderTreeFlat t = preOrderTreeFlat' [] 0 t  where
  preOrderTreeFlat' :: [FilePath] -> TreeDepth -> TreeObj -> [TreeNodeInfo]
  preOrderTreeFlat' path depth (TreeObjRoot pel) = (preOrderTreeFlat' path (depth + 1) =<< pel)
  preOrderTreeFlat' path depth (TreeObjBranch pe pel) = 
    (pe, Tree, normalizePath ((T.unpack (pFileName pe)):path), depth):(pel >>= 
      (preOrderTreeFlat' ((T.unpack $ pFileName pe):path) (depth + 1)))
  preOrderTreeFlat' p d (TreeObjLeaf pe _) = return
    (pe, Blob, normalizePath ((T.unpack $ pFileName pe):p), d)
  normalizePath = intercalate "/" . reverse

treeToText :: TreeListObj -> IO T.Text
treeToText t = expandTree t >>= \et ->
  return $ T.intercalate "\n" $ (treeNodeInfoToText <$> (preOrderTreeFlat et)) where
  treeNodeInfoToText (pathEntry, ot, fp, _) = T.concat
    [ 
      normalizeDigLength (T.pack $ show $ pFileMode pathEntry) 6
    , " "
    , objTypeToText ot
    , " "
    , pHash pathEntry
    , "    "
    , T.pack fp
    ]

normalizeDigLength :: T.Text -> Int ->  T.Text
normalizeDigLength num len =
  if (T.length num < len) then
      normalizeDigLength (T.concat ["0",num]) len
  else
    num

serializePathEntry :: PathEntry -> BS.ByteString
serializePathEntry (PathEntry fmode fname fhash) =
  BS.concat [
             C8.pack $ show fmode
            , " "
            , T.encodeUtf8 fname
            , "\0"
            , fst $ B16.decode $ T.encodeUtf8 fhash
            ]

treeFormat :: TreeParser
treeFormat = TreeListObj <$> some pathFormat <* MP.eof

pathFormat :: PathParser
pathFormat =
  PathEntry <$> (MP.decimal)
            <*   MP.space1
            <*> (T.decodeUtf8 . BS.pack <$> some MP.printChar)
            <*  (MP.char 0)
            <*> (T.decodeUtf8 . B16.encode <$> MP.takeP Nothing hashlen)
              where hashlen = 20
