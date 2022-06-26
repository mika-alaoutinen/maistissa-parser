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

testStr = "VIINI: Apothic Dark 2015\nMaa: Espanja\nHinta: 13.49\nKuvaus: Pehmeä ja hedelmäinen, täyteläinen"

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
    descriptions = splitOn ", " <$> anyString

-- Parse wine properties
winePropertyParser :: Parser WineProperty
winePropertyParser = nameParser <|> countryParser <|> priceParser <|> descriptionParser

parseWineProperties :: Parser [WineProperty]
parseWineProperties = winePropertyParser `separatedBy` newline

commaSeparated :: String -> [String]
commaSeparated = splitOn ", "