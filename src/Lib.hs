module Lib (fileToLine,
            fileToLines,
            apply) where

import Data.List (lines)

fileToLines :: FilePath -> IO [String]
fileToLines path = do text <- readFile path
                      return $ lines text

fileToLine :: FilePath -> IO String
fileToLine path = fmap head $ fileToLines path

apply :: Show b => (a -> b) -> IO a -> IO String
apply f contents = fmap (show . f) contents
