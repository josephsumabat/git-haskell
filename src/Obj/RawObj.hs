{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Obj.RawObj (
  RawObj (..)
  , ObjType(..)
  , GitObj (..)
  , ObjHash
  , parseObj
  , objTypeToText
  , objContents
  , objHash
  , objTypeFormat
  ) where

import Codec.Compression.Zlib (decompress)
import Control.Exception (throw)
import Text.Read (readMaybe)
import DefinedExceptions (ObjException(..), maybeExceptionHelper)

import qualified Data.Text as T (Text, concat, pack)
import Data.Text.Encoding as T (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Lazy 
  as LBS (ByteString, toStrict, fromStrict)
import qualified Data.ByteString.Char8
  as C (break, unpack, drop)
import qualified Text.Megaparsec as P (Parsec)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.ByteString.Base16 as B16 (encode)
import Crypto.Hash.SHA1 (hashlazy)
import Data.Void (Void)

import Control.Applicative

import DefinedExceptions (maybeExceptionHelper, ObjException(..))

type ObjHash = T.Text

data RawObj = RawObj {
        objType :: ObjType
      , size :: Int
      , rawContents :: T.Text
  } deriving Show

data ObjType = Blob | Commit | Tree deriving Show

class GitObj a where
  fromRawMaybe::RawObj -> Maybe a
  toRaw::a -> RawObj
  fromRaw::RawObj -> a
  fromRaw = maybeExceptionHelper fromRawMaybe UnexpectedObjectException

type ObjTypeParser= P.Parsec Void T.Text ObjType

objTypeToText::ObjType -> T.Text
objTypeToText Blob = "blob"
objTypeToText Commit = "commit"
objTypeToText Tree = "tree"

textToObjTypeMaybe::T.Text -> Maybe ObjType
textToObjTypeMaybe "blob" = Just Blob
textToObjTypeMaybe "commit" = Just Commit
textToObjTypeMaybe "tree" = Just Tree
textToObjTypeMaybe _  = Nothing

textToObjType::T.Text -> ObjType
textToObjType = maybeExceptionHelper textToObjTypeMaybe ParseException

-- Custom parser to get object contents
-- TODO: Replace with megaparsec parser
-- This function is effective a parseMaybe on a RawObj parser
parseObjMaybe::LBS.ByteString -> Maybe RawObj
parseObjMaybe contents =
  let !decompressedContents = LBS.toStrict $ decompress contents
      -- Split file into metadata and content from first nullbyte
      !splitData = C.break ('\0'==) decompressedContents 
      -- Split metadata into object type and size from first space
      !metaData =  C.break (' '==) $ fst splitData
      in 
        RawObj 
        <$> (textToObjTypeMaybe $ T.decodeUtf8 $ fst metaData)
        <*> (readMaybe $ C.unpack $ C.drop 1 $ snd metaData)
        <*> (return $ T.decodeUtf8 $ C.drop 1 $ snd splitData)

parseObj::LBS.ByteString -> RawObj
parseObj contents =
  case parseObjMaybe contents of
    Nothing -> throw ParseException
    Just o -> o

objContents::RawObj -> LBS.ByteString
objContents (RawObj t s c) =
  LBS.fromStrict $ T.encodeUtf8 $ T.concat
    [(objTypeToText t) ," " , (T.pack $ show s) , "\0" ,  c]

objHash::RawObj -> ObjHash
objHash = decodeUtf8 . B16.encode . hashlazy . objContents

objTypeFormat::ObjTypeParser 
objTypeFormat = (textToObjType <$> T.pack <$> some alphaNumChar) <* (char '\0')
