module WineParser where

import Lines (trim)
import Parser.Parser
import qualified Parser.Predicates as P

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
  let parser = P.string $ show key
  (_, name) <- runParser parser input
  return (trim name)

parseName :: String -> Maybe WineProperty
parseName input = do
  let parser = P.withPrefix_ "VIINI"
  (name, unparsed) <- runParser parser input
  return $ Name name

parseCountry :: String -> Maybe WineProperty
parseCountry input = do
  let parser = P.withPrefix_ "Maa"
  (country, unparsed) <- runParser parser input
  return $ Country country
