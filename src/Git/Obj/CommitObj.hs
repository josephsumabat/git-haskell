{-# LANGUAGE OverloadedStrings #-}
module Git.Obj.CommitObj where

import           Data.Void                  (Void)
import           Control.Applicative         (some, optional)

import qualified Data.ByteString            as BS  (ByteString, concat, pack)
import qualified Text.Megaparsec            as MP (Parsec, eof)
import qualified Text.Megaparsec.Byte       as MP (string, space1, hexDigitChar, printChar, eol)
import qualified Data.Text                  as T  (Text, pack)
import qualified Data.Text.Encoding         as T   (encodeUtf8, decodeUtf8)

import Git.Obj.RawObj (ObjType(..), ObjHash, objTypeFormat)

data CommitObj = CommitObj {
    cTree      :: T.Text
  , cParents   :: [ ObjHash ]
  , cAuthor    :: T.Text
  --, cDate      :: T.Text
  , cCommitter :: T.Text
  , cMessage   :: T.Text
} deriving (Show)

type CommitParser = MP.Parsec Void BS.ByteString CommitObj

type CommitKvParser = MP.Parsec Void BS.ByteString T.Text

kHashFormat :: T.Text -> CommitKvParser
kHashFormat key =
  (MP.string $ T.encodeUtf8 key)
  *> (some MP.space1)
  *> (T.decodeUtf8 . BS.pack <$>  (some MP.hexDigitChar))
  <* optional MP.eol

kNameDateFormat :: T.Text -> CommitKvParser
kNameDateFormat key =
  (MP.string $ T.encodeUtf8 key)
  *> (T.decodeUtf8 . BS.pack <$>  (some MP.printChar))
  <* optional MP.eol

commitFormat :: CommitParser
commitFormat =
  CommitObj
    <$> kHashFormat "tree"
    <*> (some $ kHashFormat "parent")
    <*> kNameDateFormat "author"
    <*> kNameDateFormat "committer"
    <*  MP.eol
    <*> (T.decodeUtf8 . BS.pack <$> (some MP.printChar))

