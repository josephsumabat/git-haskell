module Obj.BlobObj (
    BlobObj(BlobObj)
  , fromRaw
  , toRaw
  , makeBlob
) where

import Obj.RawObj (
  RawObj(RawObj)
  , ObjType(Blob)
  , GitObj(fromRaw , toRaw) 
  , rawContents
  )
import qualified Data.ByteString.Lazy 
  as LBS (ByteString, toStrict, length)
import qualified Data.ByteString.Lazy.UTF8
  as LUTF8 (fromString)
import qualified Data.ByteString.Char8
  as C (unpack)

data BlobObj = BlobObj { blobContents::String }

instance GitObj BlobObj where
  fromRaw r = BlobObj $ rawContents r
  toRaw b = let c = blobContents b in
    RawObj Blob (fromIntegral $ LBS.length $ LUTF8.fromString c) c

makeBlob::LBS.ByteString -> BlobObj
makeBlob = BlobObj . C.unpack . LBS.toStrict
