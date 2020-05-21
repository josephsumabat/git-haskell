{-# LANGUAGE OverloadedStrings #-}
module Git.Obj.TreeObj (
    TreeListObj(..)
  , FileObjType(..)
  , PathEntry(..)
  , pathFormat
  , treeFormat
  --, expandTree
  , objToFileType
) where

import           Control.Applicative         (optional, some)
import           Data.Void                  (Void)

import qualified Data.Text                  as T  (Text, pack, concat)
import qualified Data.Text.Encoding         as T  (encodeUtf8)
import qualified Text.Megaparsec            as MP (Parsec, parse, takeP, eof, single, take1_)
import qualified Text.Megaparsec.Byte       as MP (alphaNumChar
                                                  , alphaNumChar
                                                  , char
                                                  , digitChar
                                                  , eol
                                                  , printChar
                                                  , space1)
import qualified Text.Megaparsec.Byte.Lexer as MP (decimal)
import qualified Text.Megaparsec.Char       as MPC (alphaNumChar, char, digitChar,  eol, space1)
import qualified Text.Megaparsec.Char.Lexer as MPC (decimal)
import qualified Data.ByteString            as BS (ByteString, length, singleton, pack)

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
  , pFileName  :: BS.ByteString
  , pHash      :: BS.ByteString
  } | PathRoot deriving (Eq, Show)

data FileObjType = BlobFileType | TreeFileType deriving (Eq, Show)
data TreeObj = TreeObjLeaf PathEntry BlobObj | TreeObjBranch PathEntry [ TreeObj ]

type FileName = String
type TreeParser = MP.Parsec Void BS.ByteString TreeListObj
type PathParser = MP.Parsec Void BS.ByteString PathEntry

--instance GitObj TreeListObj where
--  fromRawMaybe (RawObj Tree _ r) = rightToMaybe (MP.parse treeFormat "" r)
--  toRaw t = let c = treeToText t in
--                RawObj Tree (fromIntegral $ BS.length $ T.encodeUtf8 c) c

objToMaybeFileType :: ObjType -> Maybe FileObjType
objToMaybeFileType Blob = Just BlobFileType
objToMaybeFileType Tree = Just TreeFileType
objToMaybeFileType _    = Nothing

objToFileType :: ObjType -> FileObjType
objToFileType =
  maybeExceptionHelper objToMaybeFileType UnexpectedObjectException

--expandTree :: TreeListObj -> IO TreeObj
--expandTree t = expandTree' PathRoot t where
--  expandTree' :: PathEntry -> TreeListObj -> IO TreeObj
--  expandTree' p l = (TreeObjBranch p) <$> (traverse expandPath $ files l) 
--  expandPath :: PathEntry -> IO TreeObj
--  expandPath path = 
--    case path of
--  -- Blob case
--      (PathEntry _ BlobFileType h _) -> TreeObjLeaf path <$> fromRaw <$> readObj h 
--  -- Tree case
--      (PathEntry _ TreeFileType h _) -> (expandTree' path) =<< (fromRaw <$> (readObj $ h)) 

-- getObjRepresentation :: TreeListObj -> [TreeObj]
-- getObjRepresentation t = fmap

--treeToText :: TreeListObj -> T.Text
--treeToText (TreeListObj f) = T.concat $ pathEntryToText <$> f

--pathEntryToText :: PathEntry -> T.Text
--pathEntryToText (PathEntry fmode ftype fhash fname) =
--  T.concat [ T.pack $ show fmode
--           , " "
--           , fileTypeToText ftype
--           , " "
--           , fhash
--           , "    "
--           , T.pack fname
--           , "\n"
--           ]

fileTypeToText :: FileObjType -> T.Text
fileTypeToText BlobFileType = "blob"
fileTypeToText TreeFileType = "tree"

treeFormat :: TreeParser
treeFormat = TreeListObj <$> some pathFormat


pathFormat :: PathParser
pathFormat =
  PathEntry <$> (MP.decimal)
            <*   MP.space1
            <*> (BS.pack <$> (some MP.printChar))
            <*  (MP.char 0)
            <*> ((MP.takeP Nothing 20)) 
              where

-- pathFormat :: PathParser
-- pathFormat =
--   PathEntry
--     <$> MP.decimal
--     <*  (MP.space1)
--     <*> (objToFileType <$> objTypeFormat)
--     <*  (MP.space1)
--     <*> (T.pack <$> some MP.alphaNumChar)
--     <*  (MP.space1)
--     <*> (some MP.alphaNumChar)
--     <*  (optional MP.eol)
--     <*  (optional MP.eof)
