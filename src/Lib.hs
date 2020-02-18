module Lib
    ( someFunc
    ) where

import Codec.Compression.Zlib (compress, decompress)
import Obj (objType, parseObj, rawContents, makeObjContents, makeRawBlob)
import Errors (validGitPath)
import Dir (gitPath, hashToFile)
import qualified Data.ByteString.Lazy as LBS (writeFile, readFile, toStrict)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Data.ByteString.Char8
  as C (pack, unpack)
import Data.ByteString.UTF8 as UTF8 (fromString, toString)
import Crypto.Hash.SHA1 (hashlazy)
import Data.ByteString.Base16 as B16 (encode)


someFunc :: IO ()
someFunc = do
  --x <- LBS.readFile "./.git/objects/1d/f38eff158d0a8dca58932927b7fdf17504eed5"
  --x <- LBS.readFile "tst.md"
  --dir <- validGitPath
  --print dir
  cmd_hashobject "README.md"
  --print x
  --print $ parseFile $ x

-- git cat-file command
cmd_catfile::String-> IO ()
cmd_catfile objectId =
  let hashFile = hashToFile objectId in
  do
    gitPath' <- validGitPath
    fileContents <- LBS.readFile $ hashFile gitPath'
    Prelude.putStrLn $ rawContents $ parseObj fileContents
    Prelude.putStrLn $ objType $ parseObj fileContents

-- git hash-object command
cmd_hashobject file =
  do
    fileContents <- LBS.readFile file
    gitPath' <- validGitPath
    let objectContents = makeObjContents $ makeRawBlob fileContents
        objhash = C.unpack $ B16.encode $ hashlazy objectContents in
        do
      --LBS.writeFile "tst.txt" $ compress objectContents
        putStrLn $ C.unpack $ B16.encode $ hashlazy objectContents

