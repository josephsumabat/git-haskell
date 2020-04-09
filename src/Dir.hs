{-# LANGUAGE BangPatterns #-}
module Dir (
    gitPath
  , hashToFile
  ) where

import System.Directory
  (
    getCurrentDirectory
  , doesDirectoryExist
  , canonicalizePath
  )

import System.FilePath
  (
    joinPath
  , equalFilePath
  )
import Control.Exception (throw)
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))

import DefinedExceptions (DirException(..))

gitDir::String
gitDir = ".git"

objectDir::String
objectDir = "objects"

doesGitDirectoryExist::String -> IO Bool
doesGitDirectoryExist dir = doesDirectoryExist $ joinPath [dir, gitDir]

-- Monad transformer that recursively searches curent directory and parents for
-- a git directory
gitPathMaybe::MaybeT IO FilePath
gitPathMaybe =
     (lift $ getCurrentDirectory)
      >>= \dir -> (lift $ doesGitDirectoryExist dir)
      >>= findGitDirectory' dir
    where
    findGitDirectory'::String -> Bool -> MaybeT IO FilePath
    findGitDirectory' !currentDir !gitExists
      | gitExists = lift $ return $ joinPath [currentDir, gitDir]
      | equalFilePath currentDir "/" = MaybeT $ return Nothing
      | otherwise = (lift $ canonicalizePath (joinPath [currentDir, ".."]))
        >>= \dir -> (lift $ doesGitDirectoryExist dir)
        >>= findGitDirectory' dir

gitPath::IO FilePath
gitPath = (runMaybeT gitPathMaybe) >>=
  \maybePath ->
    case (maybePath) of
      Nothing -> throw DirNotFoundException
      Just p -> return p

-- Get an object's actual file
hashToFile::String -> FilePath -> FilePath
hashToFile hash gitPath = joinPath [gitPath, objectDir, (take 2 hash), (drop 2 hash)]
