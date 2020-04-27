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
import qualified Data.ByteString 
  as BS (length)
import qualified Data.ByteString.Lazy 
  as LBS (ByteString, toStrict)

import qualified Data.Text as T (Text)
import Data.Text.Encoding as T (decodeUtf8, encodeUtf8)

data BlobObj = BlobObj { blobContents::T.Text}

instance GitObj BlobObj where
  fromRaw r = BlobObj $ rawContents r
  toRaw b = let c = blobContents b in
    RawObj Blob (fromIntegral $ BS.length $ T.encodeUtf8 c) c

makeBlob::LBS.ByteString -> BlobObj
makeBlob = BlobObj . T.decodeUtf8 . LBS.toStrict
