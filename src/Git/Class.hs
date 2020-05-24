module Git.Class (GitMonad(..)) where

import qualified Data.ByteString.Lazy as LBS (readFile)
import qualified Data.Text            as T (unpack)
import           Git.Dir              (gitPath, hashToFile)
import           Git.Obj.RawObj       (ObjHash, RawObj(..), parseObj)

class (Functor m, Applicative m, Monad m) => GitMonad m where
  readObj :: ObjHash -> m RawObj


instance GitMonad IO where
  readObj objHashId = do
    objFile <- gitPath >>= return . (hashToFile $ T.unpack objHashId)
    fileContents <- LBS.readFile objFile
    return $ parseObj fileContents
