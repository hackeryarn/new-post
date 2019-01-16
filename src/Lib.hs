{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lib
    ( newFile
    ) where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Text (Text)
import qualified Data.Text.IO as IO
import qualified Data.Text as T
import Data.Char (isNumber)


newFile :: String -> IO ()
newFile file = do
  content <- IO.readFile file
  currentDate <- getCurrentTime >>= return . toGregorian . utctDay
  let ls = T.lines content
      day = show $ getDay file + 1
      newFileName = "day" ++ day ++ ".md"
      newContent = map (setDay day). map (setDate currentDate) $ deleteBody ls 
   in do
      IO.writeFile newFileName $ T.unlines newContent


setDate :: (Integer, Int, Int) -> Text -> Text
setDate date line
  | T.isPrefixOf "date: " line = T.append "date: " $ getDateText date
  | otherwise = line

getDateText :: (Integer, Int, Int) -> Text
getDateText (year, month, day) =
  T.intercalate "-" $ map (T.pack . show) [year, toInteger month, toInteger day]

setDay :: String -> Text -> Text
setDay day line
  | T.isPrefixOf "day: " line = T.append "day: " $ T.pack day 
  | otherwise = line


deleteBody :: [Text] -> [Text]
deleteBody [] = []
deleteBody (h:t) = h : (takeWhile (/= "---") t) ++ ["---"]

getDay :: String -> Int
getDay = read . filter isNumber
