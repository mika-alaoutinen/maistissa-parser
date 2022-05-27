module WineParser where

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

-- Test properties
wineName = "VIINI: Apothic Dark 2015"

wineCountry = "Maa: Yhdysvallat"

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
