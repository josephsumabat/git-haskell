{-# LANGUAGE OverloadedStrings #-}
module Git.Obj.TreeObj (
    TreeListObj(..)
  , FileObjType(..)
  , PathEntry(..)
  , pathFormat
  , treeFormat
  , expandTree
  , objToFileType
) where

import           Control.Applicative        (optional, some)
import           Data.Void                  (Void)

import qualified Data.Text                  as T  (Text, pack, concat)
import qualified Data.Text.Encoding         as T  (encodeUtf8)
import qualified Text.Megaparsec            as MP (Parsec, parse, eof)
import qualified Text.Megaparsec.Char       as MP (alphaNumChar, eol, space1)
import qualified Text.Megaparsec.Char.Lexer as MP (decimal)
import qualified Data.ByteString            as BS (length)

import           Git.DefinedExceptions          (ObjException (..)
                                                , rightToMaybe
                                                , maybeExceptionHelper)
import           Git.Obj.BlobObj                (BlobObj (..))
import           Git.Obj.RawObj                 (ObjHash, ObjType (..),
                                                 GitObj (..), RawObj (..), objTypeFormat)
import           Git.Obj.Db                     (readObj)

newtype TreeListObj =
  TreeListObj {
  files :: [PathEntry]
} deriving (Show)

data PathEntry = PathEntry
  {
    pFileMode  :: Int
  , pFileType  :: FileObjType
  , pHash      :: ObjHash
  , pFileName  :: FileName
  } | PathRoot deriving (Eq, Show)

data FileObjType = BlobFileType | TreeFileType deriving (Eq, Show)
data TreeObj = TreeObjLeaf PathEntry BlobObj | TreeObjBranch PathEntry [ TreeObj ]

type FileName = String
type TreeParser = MP.Parsec Void T.Text TreeListObj
type PathParser = MP.Parsec Void T.Text PathEntry

instance GitObj TreeListObj where
  fromRawMaybe (RawObj Tree _ r) = rightToMaybe (MP.parse treeFormat "" r)
  toRaw t = let c = treeToText t in
                RawObj Tree (fromIntegral $ BS.length $ T.encodeUtf8 c) c

objToMaybeFileType :: ObjType -> Maybe FileObjType
objToMaybeFileType Blob = Just BlobFileType
objToMaybeFileType Tree = Just TreeFileType
objToMaybeFileType _    = Nothing

objToFileType :: ObjType -> FileObjType
objToFileType =
  maybeExceptionHelper objToMaybeFileType UnexpectedObjectException

expandTree :: PathEntry -> TreeListObj -> IO TreeObj
expandTree p l = (TreeObjBranch p) <$> (traverse expandPath $ files l) where
  expandPath :: PathEntry -> IO TreeObj
  expandPath path = 
    case path of
  -- Blob case
      (PathEntry _ BlobFileType h _) -> TreeObjLeaf path <$> fromRaw <$> readObj h 
  -- Tree case
      (PathEntry _ TreeFileType h _) -> (expandTree path) =<< (fromRaw <$> (readObj $ h)) 

-- getObjRepresentation :: TreeListObj -> [TreeObj]
-- getObjRepresentation t = fmap

treeToText :: TreeListObj -> T.Text
treeToText (TreeListObj f) = T.concat $ pathEntryToText <$> f

pathEntryToText :: PathEntry -> T.Text
pathEntryToText (PathEntry fmode ftype fhash fname) =
  T.concat [ T.pack $ show fmode
           , " "
           , fileTypeToText ftype
           , "\0"
           , " "
           , fhash
           , "    "
           , T.pack fname
           , "\n"
           ]

fileTypeToText :: FileObjType -> T.Text
fileTypeToText BlobFileType = "blob"
fileTypeToText TreeFileType = "tree"

treeFormat :: TreeParser
treeFormat = TreeListObj <$> some pathFormat

pathFormat :: PathParser
pathFormat =
  PathEntry
    <$> MP.decimal
    <*  (MP.space1)
    <*> (objToFileType <$> objTypeFormat)
    <*  (MP.space1)
    <*> (T.pack <$> some MP.alphaNumChar)
    <*  (MP.space1)
    <*> (some MP.alphaNumChar)
    <*  (optional MP.eol)
    <*  (optional MP.eof)
