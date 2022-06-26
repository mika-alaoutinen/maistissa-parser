module WineParser where

import Control.Applicative (Alternative ((<|>)))
import Parser.Combinators (separatedBy)
import Parser.Parser (Parser (..))
import Parser.Predicates.Lines (newline, parseDouble, parseString, parseStrings)

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
nameParser = Name <$> parseString "VIINI"

countryParser :: Parser WineProperty
countryParser = Country <$> parseString "Maa"

priceParser :: Parser WineProperty
priceParser = Price <$> parseDouble "Hinta"

descriptionParser :: Parser WineProperty
descriptionParser = Description <$> parseStrings "Kuvaus"

foodPairingsParser :: Parser WineProperty
foodPairingsParser = FoodPairings <$> parseStrings "SopiiNautittavaksi"

-- Parse wine properties
winePropertyParser :: Parser WineProperty
winePropertyParser = nameParser <|> countryParser <|> priceParser <|> descriptionParser <|> foodPairingsParser

parseWineProperties :: Parser [WineProperty]
parseWineProperties = winePropertyParser `separatedBy` newline
