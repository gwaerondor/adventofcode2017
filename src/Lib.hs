module Lib (fileToLines, apply) where
import Data.List (lines)

fileToLines :: FilePath -> IO [String]
fileToLines path = do text <- readFile path
                      return $ lines text

apply :: Show b => (a -> b) -> IO a -> IO String
apply f contents = fmap (show . f) contents
