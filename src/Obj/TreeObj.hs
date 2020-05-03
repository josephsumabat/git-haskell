{-# LANGUAGE OverloadedStrings #-}
module Obj.TreeObj (
    TreeObj(..)
  , FileObjType(..)
  , pathFormat
  , treeFormat
  , objToFileObj
) where

import           Control.Applicative        (optional, some)
import           Data.Void                  (Void)

import qualified Data.Text                  as T (Text, pack)
import qualified Text.Megaparsec            as MP (Parsec, eof)
import qualified Text.Megaparsec.Char       as MP (alphaNumChar, eol, space1)
import qualified Text.Megaparsec.Char.Lexer as MP (decimal)

import           DefinedExceptions          (ObjException (..),
                                             maybeExceptionHelper)
import           Obj.BlobObj                (BlobObj (..))
import           Obj.RawObj                 (ObjHash, ObjType (..),
                                             objTypeFormat)

newtype TreeObj =
  TreeObj {
  files :: [PathEntry]
} deriving (Show)

data PathEntry = PathEntry
  { 
    fileMode  :: Int
  , fileType  :: FileObjType
  , pHash     :: ObjHash
  , fileName  :: FileName
  } deriving (Show)

data FileObjType = BlobFileObj | TreeFileObj deriving (Show)

data FileObj = FileObjBlob BlobObj | FileObjTree TreeObj

type FileName = String
type TreeParser = MP.Parsec Void T.Text TreeObj
type PathParser = MP.Parsec Void T.Text PathEntry

objToMaybeFileObj :: ObjType -> Maybe FileObjType
objToMaybeFileObj Blob = Just BlobFileObj
objToMaybeFileObj Tree = Just TreeFileObj
objToMaybeFileObj _    = Nothing

objToFileObj :: ObjType -> FileObjType
objToFileObj =
  maybeExceptionHelper objToMaybeFileObj UnexpectedObjectException

treeFormat :: TreeParser
treeFormat = TreeObj <$> some pathFormat

pathFormat :: PathParser
pathFormat =
  PathEntry
    <$> MP.decimal
    <*  (MP.space1)
    <*> (objToFileObj <$> objTypeFormat)
    <*  (MP.space1)
    <*> (T.pack <$> some MP.alphaNumChar)
    <*  (MP.space1)
    <*> (some MP.alphaNumChar)
    <*  (optional MP.eol)
    <*  (optional MP.eof)
