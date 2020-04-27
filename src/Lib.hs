module Lib
    ( someFunc
    ) where

import Obj.RawObj (
    GitObj(..)
  , objType
  , parseObj
  , rawContents
  , makeObjContents)
import Obj.BlobObj (makeBlob)
import Dir (gitPath, hashToFile)
import qualified Data.ByteString.Lazy as LBS (ByteString, readFile)
import qualified Data.ByteString.Char8
  as C (unpack)
import Crypto.Hash.SHA1 (hashlazy)
import Data.ByteString.Base16 as B16 (encode)

someFunc :: IO ()
someFunc = do
  --x <- LBS.readFile "./.git/objects/1d/f38eff158d0a8dca58932927b7fdf17504eed5"
  --x <- LBS.readFile "tst.md"
  --dir <- validGitPath
  --print dir
  cmd_hashobject "README.md"
  hashobject "README.md" >>= cmd_catfile . hashCmdObjHash
  --print x
  --print $ parseFile $ x

-- git cat-file command
cmd_catfile::String -> IO ()
cmd_catfile objectId =
  let hashFile = hashToFile objectId in
  do
    gitPath' <- gitPath
    fileContents <- LBS.readFile $ hashFile gitPath'
    Prelude.print $ rawContents $ parseObj fileContents
    Prelude.print $ objType $ parseObj fileContents

-- git hash-object command
cmd_hashobject::String -> IO ()
cmd_hashobject file =
  hashobject file >>=  \x -> (print . hashCmdObjHash) x >> (print . hashCmdObjContents) x

hashobject::String -> IO GitHashCmd
hashobject file =
  do
    fileContents <- LBS.readFile file
    let objectContents = makeObjContents $ toRaw $ makeBlob fileContents
        objhash = C.unpack $ B16.encode $ hashlazy objectContents in
      return $ GitHashCmd objhash objectContents

data GitHashCmd = GitHashCmd {
    hashCmdObjHash::String
  , hashCmdObjContents::LBS.ByteString
}
