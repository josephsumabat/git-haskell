{-# LANGUAGE OverloadedStrings #-}
module Git.Lib
    ( someFunc
    ) where

import Git.Obj.RawObj (
    GitObj(..)
  , objType
  , parseObj
  , rawContents
  , objContents
  , objHash
  )
import Git.Obj.BlobObj (makeBlob)
import Git.Obj.Db (readObj)
import Git.Dir (gitPath, hashToFile)
import qualified Data.Text as T (Text, concat, pack, unpack)
import qualified Data.ByteString.Lazy as LBS (ByteString, readFile, toStrict)
import qualified Data.ByteString.Char8 as C (unpack)
import Crypto.Hash.SHA1 (hashlazy)
import Data.ByteString.Base16 as B16 (encode)

import Git.Obj.TreeObj
import Git.Obj.BlobObj
import Text.Megaparsec
import qualified Data.Text.IO as T

someFunc :: IO ()
someFunc = do
  --x <- LBS.readFile "./.git/objects/1d/f38eff158d0a8dca58932927b7fdf17504eed5"
  --x <- LBS.readFile "tst.md"
  --dir <- validGitPath
  --print dir
  --parseTest pathFormat
  --  "12314 blob def test\n12315 blob def test"
  o <- readObj "8808f77a90f533c022dfc1de035aa6ac4e5d0391"
  --print $ rawContents o
  --print o
  --print =<< (expandTree (fromRaw o))
  a <-(treeToText (fromRaw o::TreeListObj) 0)
  T.putStrLn a
  -- (fromRaw o)
  --cmd_hashobject "README.md"
  --hashobject "README.md" >>= cmd_catfile . hashCmdObjHash
  --cmd_catfile "d6f0925633e7f40092eb3a586a91d0451078cb53"
  --cmd_catfile "8808f77a90f533c022dfc1de035aa6ac4e5d0391"
  --print x
  --print $ parseFile $ x
  --print "123"

-- git cat-file command
cmd_catfile :: String -> IO ()
cmd_catfile objectId = do
    readObj (T.pack objectId) >>= putStrLn . T.unpack . blobContents . fromRaw

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
