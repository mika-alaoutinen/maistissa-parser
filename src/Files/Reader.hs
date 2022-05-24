module Reader where

import Data.List
import Data.Maybe

filePath = "./resources/testi.txt"

parseSecondLine :: String -> String
parseSecondLine = head . tail . lines

main :: IO ()
main = do
  str <- readFile filePath
  putStrLn $ parseSecondLine str
  putStrLn str
