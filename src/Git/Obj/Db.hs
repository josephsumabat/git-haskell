module Git.Obj.Db (readObj) where

import qualified Data.ByteString.Lazy as LBS (readFile)
import qualified Data.Text            as T (unpack)
import           Git.Dir              (gitPath, hashToFile)
import           Git.Obj.RawObj       (ObjHash, RawObj(..), parseObj)

readObj :: ObjHash -> IO RawObj
readObj objHashId = do
  objFile <- gitPath >>= return . (hashToFile $ T.unpack objHashId)
  fileContents <- LBS.readFile objFile
  return $ parseObj fileContents

--writeObj::RawObj -> IO ()
