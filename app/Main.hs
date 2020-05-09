{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lib
import System.IO

file :: String
file = "/home/lukas/workspace/haskell/azel/test/simpletest.az"

main :: IO ()
main = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  case translate parseSource file (source contents) of
    Left (err :: Error)  -> print err
    Right (result :: L1) -> print result
  -- testParse contents
  hClose handle
