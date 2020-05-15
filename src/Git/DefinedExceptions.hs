module Git.DefinedExceptions
  (
      maybeExceptionHelper
    , rightToMaybe
    , ObjException(..)
    , DirException(..)
  ) where

import Data.Maybe (fromMaybe)

import Control.Exception

data ObjException = ParseException | UnexpectedObjectException deriving Show

data DirException = DirNotFoundException deriving Show

instance Exception ObjException
instance Exception DirException

-- Replace a maybe function with a function that throws exception on nothing
maybeExceptionHelper::Exception e => (a -> Maybe b) -> e -> a -> b
maybeExceptionHelper fn exception input = 
  fromMaybe (throw exception) (fn input)

rightToMaybe::Either a b -> Maybe b
rightToMaybe (Right r) = Just r
rightToMaybe _         = Nothing
