module Git.Obj.BlobObj (
    BlobObj(..)
  , fromRaw
  , toRaw
  , makeBlob
) where

import Git.Obj.RawObj (
  RawObj(RawObj)
  , ObjType(Blob)
  , GitObj(..) 
  )

import qualified Data.ByteString      as BS   (length)
import qualified Data.ByteString.Lazy as LBS  (ByteString, toStrict)
import qualified Data.Text            as T    (Text)
import qualified Data.Text.Encoding   as T    (decodeUtf8, encodeUtf8)

newtype BlobObj = BlobObj { blobContents::T.Text}

instance GitObj BlobObj where
  fromRawMaybe (RawObj Blob _ r) = Just $ BlobObj r
  fromRawMaybe _  = Nothing
  toRaw b = let c = blobContents b in
    RawObj Blob (fromIntegral $ BS.length $ T.encodeUtf8 c) c

makeBlob::LBS.ByteString -> BlobObj
makeBlob = BlobObj . T.decodeUtf8 . LBS.toStrict
