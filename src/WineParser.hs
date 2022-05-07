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

data Key = WineName | WineCountry

instance Show Key where
  show WineName = "VIINI:"
  show WineCountry = "Maa:"

-- Test properties
wineName = "VIINI: Apothic Dark 2015"

wineCountry = "Maa: Yhdysvallat"

parseString :: Key -> String -> Maybe String
parseString key input = do
  let parser = Parser.string $ show key
  (_, name) <- Parser.parse parser input
  return (trim name)

parseName :: String -> Maybe WineProperty
parseName input = do
  name <- parseString WineName input
  return $ Name name

parseCountry :: String -> Maybe WineProperty
parseCountry input = do
  country <- parseString WineCountry input
  return $ Country country
