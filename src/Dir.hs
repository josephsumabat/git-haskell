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

import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))

gitDir::String
gitDir = ".git"

objectDir::String
objectDir = "objects"

doesGitDirectoryExist::String -> IO Bool
doesGitDirectoryExist dir = doesDirectoryExist $ joinPath [dir, gitDir]

-- Recursively search curent directory and parents for a git directory
--gitPath::IO (Maybe FilePath)
--gitPath =
--    getCurrentDirectory
--      >>= \dir -> doesGitDirectoryExist dir
--      >>= findGitDirectory' dir
--    where 
--    findGitDirectory'::String -> Bool -> IO (Maybe FilePath)
--    findGitDirectory' !currentDir !gitExists
--      | gitExists = return $ Just $ joinPath [currentDir, gitDir]
--      | equalFilePath currentDir "/" = return Nothing
--      | otherwise = canonicalizePath (joinPath [currentDir, ".."])
--        >>= \dir -> doesGitDirectoryExist dir
--        >>= findGitDirectory' dir

-- Monad transformer that recursively searches curent directory and parents for
-- a git directory
gitPath::MaybeT IO FilePath
gitPath =
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


hashToFile::String -> FilePath -> FilePath
hashToFile hash gitPath = joinPath [gitPath, objectDir, (take 2 hash), (drop 2 hash)]
