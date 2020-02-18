module Obj (
  RawObj (RawObj)
  , parseObj
  , rawContents
  , objType
  , makeObjContents
  , makeRawBlob
  ) where

import qualified Data.ByteString.Lazy 
  as LBS (ByteString, toStrict, length)
import qualified Data.ByteString.Lazy.UTF8
  as LUTF8 (fromString)
import qualified Data.ByteString.Char8
  as C (break, unpack, drop)
import Codec.Compression.Zlib (compress, decompress)
import Text.Read (readMaybe)
import Data.Map.Ordered

type Hash = String
data RawObj = RawObj {
        objType :: String
      , size :: Int
      , rawContents :: String
  } deriving Show

--data ObjType = Blob | Commit | Tree deriving Show

data BlobObj = Blob { blobContents::String }
data TreeObj = Tree { treeContents::String }

data KvObj =
    Commit { commitContents::(OMap String String)}
  | Tag { tagContents::String }

--objTypeToStr::ObjType -> String
--objTypeToStr (Blob) = "blob"
--objTypeToStr (Commit) = "commit"
--objTypeToStr (Tree) = "tree"
--objTypeToStr Nothing = ""

parseObj::LBS.ByteString -> RawObj
parseObj rawContents =
  let decompressedContents = LBS.toStrict $ decompress rawContents
      -- Split file into metadata and content from first nullbyte
      splitData = C.break ('\0'==) decompressedContents 
      -- Split metadata into object type and size from first space
      metaData =  C.break (' '==) $ fst splitData
      objType = C.unpack $ fst metaData
      size = case (readMaybe $ C.unpack $ C.drop 1 $ snd metaData) of
        Nothing -> -1
        Just i -> i
      contents = C.unpack $ C.drop 1 $ snd splitData
        in
        RawObj objType size contents

makeObjContents::RawObj -> LBS.ByteString
makeObjContents (RawObj t s c) = LUTF8.fromString $ t ++ " " ++ (show s) ++ "\0" ++  c

blobName::String
blobName = "blob"

makeRawBlob::LBS.ByteString -> RawObj
makeRawBlob fileContents =
  RawObj
    blobName 
    (fromIntegral $ LBS.length fileContents)
    (C.unpack $ LBS.toStrict fileContents)

rawObjToBlob::RawObj -> BlobObj
rawObjToBlob r = Blob $ rawContents r

blobToRawObj::BlobObj -> RawObj
blobToRawObj (Blob c) =
  RawObj blobName (fromIntegral $ LBS.length $ LUTF8.fromString c) c


--makeRawObj::RawObj -> LBS.ByteString
--makeRawObj = 
--

--makebjObj::RawObj -> LBS.ByteString
--makeObjObj obj = 

