module Obj.CommitObj where

data CommitObj = CommitObj {
    cTreeHash::String
  , cAuthor::String
  , cParentHash::String
}
