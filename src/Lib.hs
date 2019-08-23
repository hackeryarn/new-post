{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Lib
  ( newFile
  ) where

import           Data.Char          (isNumber)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as IO
import           Data.Time.Calendar
import           Data.Time.Clock
import           System.IO
import           System.Process

newFile :: String -> IO String
newFile file = do
  content <- IO.readFile file
  currentDate <- convertTime
  let ls = T.lines content
      day = show $ getDay file + 1
      newFileName = "day" ++ day ++ ".md"
      newContent =
        (map (setDay day . setDate currentDate) $ deleteBody ls)
   in do
    IO.writeFile newFileName $ T.unlines newContent
    return newFileName

convertTime :: IO (Integer, Int, Int)
convertTime = toGregorian . utctDay <$> getCurrentTime

setDate :: (Integer, Int, Int) -> Text -> Text
setDate date line
  | T.isPrefixOf "date: " line = T.append "date: " $ getDateText date
  | otherwise = line

getDateText :: (Integer, Int, Int) -> Text
getDateText (year, month, day) =
  T.intercalate "-" $
  map (T.pack . padTwo . show) [year, toInteger month, toInteger day]

padTwo :: String -> String
padTwo n = replicate (2 - length n) '0' ++ n

setDay :: String -> Text -> Text
setDay day line
  | T.isPrefixOf "day: " line = T.append "day: " $ T.pack day
  | otherwise = line

deleteBody :: [Text] -> [Text]
deleteBody []    = []
deleteBody (h:t) = h : takeWhile (/= "---") t ++ ["---"]

getDay :: String -> Int
getDay = read . filter isNumber

getEntry :: IO [Text]
getEntry =
  let opts = ["-on", "yesterday", "@reading"]
   in do (_, Just hout, _, _) <-
           createProcess (proc "jrnl" opts) {std_out = CreatePipe}
         contents <- hGetContents hout
         return .
           tail . map (T.pack . unwords . tail . words) . filter (/= "") . lines $
           contents
