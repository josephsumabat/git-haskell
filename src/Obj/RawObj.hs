{-# LANGUAGE OverloadedStrings #-}
module Obj.RawObj (
  RawObj (RawObj)
  , ObjType(Blob, Commit, Tree)
  , GitObj (fromRaw, toRaw)
  , parseObj
  , rawContents
  , objType
  , objTypeToText
  , makeObjContents
  ) where

import Codec.Compression.Zlib (compress, decompress)
import Control.Exception (throw)
import Text.Read (readMaybe)

import qualified Data.Text as T (Text, concat, pack)
import Text.Show as T (Show(..))
import Data.Text.Encoding as T (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Lazy 
  as LBS (ByteString, toStrict, fromStrict, length)
import qualified Data.ByteString.Lazy.UTF8
  as LUTF8 (fromString)
import qualified Data.ByteString.Char8
  as C (break, unpack, drop)

import DefinedExceptions (ObjException(..))

test=1
type Hash = String
data RawObj = RawObj {
        objType :: ObjType
      , size :: Int
      , rawContents :: T.Text
  } deriving Show

data ObjType = Blob | Commit | Tree deriving Show

class GitObj a where
  fromRaw::RawObj -> a
  toRaw::a -> RawObj

objTypeToText::ObjType -> T.Text
objTypeToText Blob = "blob"
objTypeToText Commit = "commit"
objTypeToText Tree = "tree"

strToObjType::T.Text -> Maybe ObjType
strToObjType "blob" = Just Blob
strToObjType "commit" = Just Commit
strToObjType "tree" = Just Tree
strToObjType _  = Nothing

parseObjMaybe::LBS.ByteString -> Maybe RawObj
parseObjMaybe rawContents =
  let decompressedContents = LBS.toStrict $ decompress rawContents
      -- Split file into metadata and content from first nullbyte
      splitData = C.break ('\0'==) decompressedContents 
      -- Split metadata into object type and size from first space
      metaData =  C.break (' '==) $ fst splitData
      in 
        RawObj 
        <$> (strToObjType $ T.decodeUtf8 $ fst metaData)
        <*> (readMaybe $ C.unpack $ C.drop 1 $ snd metaData)
        <*> (return $ T.decodeUtf8 $ C.drop 1 $ snd splitData)

parseObj::LBS.ByteString -> RawObj
parseObj rawContents =
  case parseObjMaybe rawContents of
    Nothing -> throw ParseException
    Just o -> o

makeObjContents::RawObj -> LBS.ByteString
makeObjContents (RawObj t s c) =
  LBS.fromStrict $ T.encodeUtf8 $ T.concat
    [(objTypeToText t) ," " , (T.pack $ show s) , "\0" ,  c]
