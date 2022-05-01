module Main where

import Data.Maybe (fromMaybe)
import WineParser (WineProperty (Name), parseName)

wineName = "VIINI: Apothic Dark 2015"

main :: IO ()
main = do
  let (Name name) = fromMaybe (Name "No name") $ parseName wineName
  putStrLn $ "Name: " ++ name
