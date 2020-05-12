{-# LANGUAGE BangPatterns #-}
module Git.Dir
  (
    gitPath
  , hashToFile
  ) where

import System.Directory          (canonicalizePath,
                                  doesDirectoryExist,
                                  getCurrentDirectory)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import System.FilePath           (equalFilePath, joinPath)

import Git.DefinedExceptions         (DirException (..), maybeExceptionHelper)

gitDir :: String
gitDir = ".git"

objectDir :: String
objectDir = "objects"

doesGitDirectoryExist :: String -> IO Bool
doesGitDirectoryExist dir = doesDirectoryExist $ joinPath [dir, gitDir]

-- Monad transformer that recursively searches curent directory and parents for
-- a vc directory
vcPathMaybe :: FilePath -> MaybeT IO FilePath
vcPathMaybe pathname =
  (lift $ getCurrentDirectory)
      >>= \dir -> (lift $ doesGitDirectoryExist dir)
      >>= findGitDirectory' dir
        where
      findGitDirectory' :: String -> Bool -> MaybeT IO FilePath
      findGitDirectory' !currentDir !gitExists
        | gitExists = lift $ return $ joinPath [currentDir, pathname]
        | equalFilePath currentDir "/" = MaybeT $ return Nothing
        | otherwise = (lift $ canonicalizePath (joinPath [currentDir, ".."]))
        >>= \dir -> (lift $ doesGitDirectoryExist dir)
          >>= findGitDirectory' dir

gitPathMaybe :: MaybeT IO FilePath
gitPathMaybe = vcPathMaybe gitDir

gitPath :: IO FilePath
gitPath = (runMaybeT gitPathMaybe) >>=
  \maybePath -> return $ maybeExceptionHelper (const maybePath) DirNotFoundException ()

-- Get an object's actual file
hashToFile :: String -> FilePath -> FilePath
hashToFile hash vcPath = joinPath [vcPath, objectDir, (take 2 hash), (drop 2 hash)]
