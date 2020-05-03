module Obj.CommitObj where

import qualified Text.Megaparsec            as MP (Parsec, eof)
import qualified Data.Text                  as T  (Text, pack)

import Obj.RawObj (ObjType(..), ObjHash, objTypeFormat)

data CommitObj = CommitObj {
    cTreeHash::T.Text
  , cAuthor::T.Text
  , cParentHash::ObjHash
  , cDate::T.Text
}
