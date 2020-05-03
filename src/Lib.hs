{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Obj.RawObj (
    GitObj(..)
  , objType
  , parseObj
  , rawContents
  , objContents
  , objHash
  )
import Obj.BlobObj (makeBlob)
import Obj.Db (readObj)
import Dir (gitPath, hashToFile)
import qualified Data.Text as T (Text, concat, pack, unpack)
import qualified Data.ByteString.Lazy as LBS (ByteString, readFile)
import qualified Data.ByteString.Char8 as C (unpack)
import Crypto.Hash.SHA1 (hashlazy)
import Data.ByteString.Base16 as B16 (encode)

import Obj.TreeObj
import Text.Megaparsec

someFunc :: IO ()
someFunc = do
  --x <- LBS.readFile "./.git/objects/1d/f38eff158d0a8dca58932927b7fdf17504eed5"
  --x <- LBS.readFile "tst.md"
  --dir <- validGitPath
  --print dir
  parseTest pathFormat
    "12314 blob\0 def test\n12315 blob def test"
  parseTest treeFormat 
    "12314 blob\0 def test\n22315 blob\0 def testef"
  cmd_hashobject "README.md"
  hashobject "README.md" >>= cmd_catfile . hashCmdObjHash
  --print x
  --print $ parseFile $ x

-- git cat-file command
cmd_catfile :: String -> IO ()
cmd_catfile objectId = do
    readObj (T.pack objectId) >>= putStrLn . T.unpack . rawContents

-- git hash-object command
cmd_hashobject :: String -> IO ()
cmd_hashobject file =
  hashobject file >>=  \x -> (print . hashCmdObjHash) x >> (print . hashCmdObjContents) x

hashobject :: String -> IO GitHashCmd
hashobject file =
  do
    fileContents <- LBS.readFile file
    let objectContents = objContents $ toRaw $ makeBlob fileContents
        objhash = C.unpack $ B16.encode $ hashlazy objectContents in
      return $ GitHashCmd objhash objectContents

data GitHashCmd = GitHashCmd {
    hashCmdObjHash :: String
  , hashCmdObjContents :: LBS.ByteString
}
