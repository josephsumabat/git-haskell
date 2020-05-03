{-# LANGUAGE OverloadedStrings #-}
module Obj.TreeObj (
    TreeObj(..)
  , FileObjType(..)
  , pathFormat
  , treeFormat
  , objToMaybeFileObj
) where

import Obj.RawObj (ObjType(..), ObjHash, objTypeFormat)

import Control.Applicative
import Text.Megaparsec (Parsec, optional, eof, token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void (Void)
import qualified Data.Text as T (Text, pack)

newtype TreeObj = TreeObj {
  files::[PathEntry]
} deriving (Show)

data PathEntry = PathEntry 
  {   fileMode::Int
    , fileType::ObjType
    , hash::ObjHash
    , fileName::FileName
  } deriving (Show)

data FileObjType = BlobFileObj | TreeFileObj

type FileName = String
type TreeParser = Parsec Void T.Text TreeObj
type PathParser = Parsec Void T.Text PathEntry

objToMaybeFileObj::ObjType -> Maybe FileObjType
objToMaybeFileObj Blob = Just BlobFileObj
objToMaybeFileObj Tree = Just TreeFileObj
objToMaybeFileObj _ = Nothing

treeFormat::TreeParser
treeFormat = TreeObj <$> some pathFormat

pathFormat::PathParser
pathFormat =
  PathEntry
    <$> decimal
    <*  (space1)
    <*> objTypeFormat
    <*  (space1)
    <*> (T.pack <$> some alphaNumChar)
    <*  (space1)
    <*> (some alphaNumChar)
    <*  (optional eol)
    <*  (optional eof)
