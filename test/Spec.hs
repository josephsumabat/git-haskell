{-# LANGUAGE OverloadedStrings #-}
import Obj.TreeObj
import Data.Text
import Text.Megaparsec
import Test.HUnit

main :: IO ()
main = do
  testPathFormat

pathEntry :: PathEntry
pathEntry = PathEntry {pFileMode = 12314, pFileType = BlobFileType, pHash = "def", pFileName = "test"}

pathEntryStr :: Text
pathEntryStr = "12314 blob\0 def test\n12315 blob def test"

testPathFormat :: Assertion
testPathFormat = assertEqual "" (Right pathEntry) (parse pathFormat "" pathEntryStr)
