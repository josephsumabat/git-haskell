module Obj.RawObj (
  RawObj (RawObj)
  , ObjType(Blob, Commit, Tree)
  , GitObj (fromRaw, toRaw)
  , parseObj
  , rawContents
  , objType
  , objTypeToStr
  , makeObjContents
  ) where

import Codec.Compression.Zlib (compress, decompress)
import Control.Exception (throw)
import Text.Read (readMaybe)

import qualified Data.ByteString.Lazy 
  as LBS (ByteString, toStrict, length)
import qualified Data.ByteString.Lazy.UTF8
  as LUTF8 (fromString)
import qualified Data.ByteString.Char8
  as C (break, unpack, drop)

import DefinedExceptions (ObjException(..))

type Hash = String
data RawObj = RawObj {
        objType :: ObjType
      , size :: Int
      , rawContents :: String
  } deriving Show

data ObjType = Blob | Commit | Tree deriving Show

class GitObj a where
  fromRaw::RawObj -> a
  toRaw::a -> RawObj

objTypeToStr::ObjType -> String
objTypeToStr Blob = "blob"
objTypeToStr Commit = "commit"
objTypeToStr Tree = "tree"

strToObjType::String -> Maybe ObjType
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
        <$> (strToObjType $ C.unpack $ fst metaData)
        <*> (readMaybe $ C.unpack $ C.drop 1 $ snd metaData)
        <*> (return $ C.unpack $ C.drop 1 $ snd splitData)

parseObj::LBS.ByteString -> RawObj
parseObj rawContents =
  case parseObjMaybe rawContents of
    Nothing -> throw ParseException
    Just o -> o

makeObjContents::RawObj -> LBS.ByteString
makeObjContents (RawObj t s c) =
  LUTF8.fromString $ (objTypeToStr t) ++ " " ++ (show s) ++ "\0" ++  c
