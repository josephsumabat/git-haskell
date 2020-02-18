module Errors(
  validGitPath
) where

import Dir (gitPath)
import System.FilePath (joinPath)
import Control.Monad.Trans.Maybe (runMaybeT)

-- The git path. If no git path throw an error
validGitPath::IO FilePath
validGitPath = do
  gitPath' <- runMaybeT gitPath
  case gitPath' of
    Nothing -> ioError $ userError "Not a valid git directory"
    Just p -> return p

--validObj::RawObj -> IO RawObj
--validObj r=
--  case objType of
--    Nothing -> ioError $ mkIOError "Not a valid object"
--    Just t ->
--      case size of
--        Nothing -> error "could not parse; invalid size"
--        Just i -> print "test"
