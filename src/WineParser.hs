module WineParser where

import Control.Applicative
import Parser.Combinators (separatedBy)
import Parser.Parser
import Parser.Predicates (newline, withPrefix)

data WineProperty
  = Name String
  | Country String
  | Price Double
  | Description [String]
  | FoodPairings [String]
  | Url (Maybe String)
  deriving (Show, Eq)

testStr = "VIINI: Apothic Dark 2015\nMaa: Espanja"

nameParser :: Parser WineProperty
nameParser = Name <$> withPrefix "VIINI"

countryParser :: Parser WineProperty
countryParser = Country <$> withPrefix "Maa"

winePropertyParser :: Parser WineProperty
winePropertyParser = nameParser <|> countryParser

parseWineProperties :: Parser [WineProperty]
parseWineProperties = winePropertyParser `separatedBy` newline
