module WineParser where

import Lines (trim)
import qualified Parser

data Wine = Wine
  { name :: String,
    country :: String,
    description :: [String],
    foodPairings :: [String]
  }
  deriving (Show, Eq)

data WineProperty
  = Name String
  | Country String
  | Price Double
  | Description [String]
  | FoodPairings [String]
  | Url (Maybe String)
  deriving (Show, Eq)

parseName :: String -> Maybe WineProperty
parseName input = do
  (prefix, name) <- Parser.parse (Parser.string "VIINI:") input
  return (Name $ trim name)

wineName = "VIINI: Apothic Dark 2015"