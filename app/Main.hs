module Main where

import Lib
import System.IO

file :: String
file = "/home/lukas/workspace/haskell/azel/test/simpletest.az"

main :: IO ()
main = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  -- case azParse file contents of
  --   Left err     -> print err
  --   Right result -> print result -- TODO pretty-print
  testParse contents
  hClose handle
  
