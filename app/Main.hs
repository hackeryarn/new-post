module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  [arg] <- getArgs
  newFile arg
