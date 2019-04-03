module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  [arg] <- getArgs
  newFileName <- newFile arg
  putStrLn newFileName
