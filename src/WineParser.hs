module WineParser where

import Control.Applicative (Alternative ((<|>)))
import Data.List.Split (splitOn)
import Parser.Combinators (separatedBy)
import Parser.Parser (Parser (..))
import Parser.Predicates.Digits (double)
import Parser.Predicates.Lines (newline, withPrefix)
import Parser.Predicates.Strings (anyString)

data WineProperty
  = Name String
  | Country String
  | Price Double
  | Description [String]
  | FoodPairings [String]
  | Url (Maybe String)
  deriving (Show, Eq)

testStr =
  "VIINI: Apothic Dark 2015\n\
  \Maa: Espanja\n\
  \Hinta: 13.49\n\
  \Kuvaus: Pehmeä ja hedelmäinen, täyteläinen\n\
  \SopiiNautittavaksi: seurustelujuomana, pikkusuolaiset, pasta ja pizza, grilliruoka"

-- Parse specific wine properties
nameParser :: Parser WineProperty
nameParser = Name <$> withPrefix "VIINI" anyString

countryParser :: Parser WineProperty
countryParser = Country <$> withPrefix "Maa" anyString

priceParser :: Parser WineProperty
priceParser = Price <$> withPrefix "Hinta" double

descriptionParser :: Parser WineProperty
descriptionParser = Description <$> withPrefix "Kuvaus" descriptions
  where
    descriptions = commaSeparated <$> anyString

foodPairingsParser :: Parser WineProperty
foodPairingsParser = FoodPairings <$> withPrefix "SopiiNautittavaksi" foodPairings
  where
    foodPairings = commaSeparated <$> anyString

-- Parse wine properties
winePropertyParser :: Parser WineProperty
winePropertyParser = nameParser <|> countryParser <|> priceParser <|> descriptionParser <|> foodPairingsParser

parseWineProperties :: Parser [WineProperty]
parseWineProperties = winePropertyParser `separatedBy` newline

-- Helpers
commaSeparated :: String -> [String]
commaSeparated = splitOn ", "