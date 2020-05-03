module Obj.Db (readObj) where

import Obj.RawObj (RawObj(..), parseObj, ObjHash)
import Dir (gitPath, hashToFile)
import qualified Data.ByteString.Lazy as LBS (ByteString, readFile)
import qualified Data.Text as T (unpack)

readObj::ObjHash -> IO RawObj
readObj objHashId = do
  objFile <- gitPath >>= return . (hashToFile $ T.unpack objHashId)
  fileContents <- LBS.readFile objFile
  return $ parseObj fileContents
  
--writeObj::RawObj -> IO ()
