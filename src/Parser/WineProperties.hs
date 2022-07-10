module Parser.WineProperties
  ( WineProperty (..),
    winePropertyParser,
    winePropertiesParser,
  )
where

import Control.Applicative ((<|>))
import Parser.Combinators (separatedBy)
import Parser.Lines (parseNumber, parseString, parseStrings)
import Parser.Parser (Parser (..))
import Parser.Predicates.Chars (newline)

data WineProperty
  = Name String
  | Country String
  | Price Double
  | Description [String]
  | FoodPairings [String]
  | Url (Maybe String)
  deriving (Show, Eq)

name :: Parser WineProperty
name = Name <$> parseString "VIINI"

country :: Parser WineProperty
country = Country <$> parseString "Maa"

price :: Parser WineProperty
price = Price <$> parseNumber "Hinta"

description :: Parser WineProperty
description = Description <$> parseStrings "Kuvaus"

foodPairings :: Parser WineProperty
foodPairings = FoodPairings <$> parseStrings "SopiiNautittavaksi"

url :: Parser WineProperty
url = Url . Just <$> parseString "url"

-- Exports
winePropertyParser :: Parser WineProperty
winePropertyParser = name <|> country <|> price <|> description <|> foodPairings <|> url

winePropertiesParser :: Parser [WineProperty]
winePropertiesParser = winePropertyParser `separatedBy` newline
