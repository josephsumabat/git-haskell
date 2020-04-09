module DefinedExceptions
  (
      ObjException(..)
    , DirException(..)
  ) where

import Control.Exception

data ObjException = ParseException deriving Show

data DirException = DirNotFoundException deriving Show

instance Exception ObjException
instance Exception DirException
