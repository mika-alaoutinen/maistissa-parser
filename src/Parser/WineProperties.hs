module Parser.WineProperties
  ( WineProperty (..),
    winePropertyParser,
    winePropertiesParser,
  )
where

import Control.Applicative ((<|>))
import Parser.Combinators (separatedBy)
import Parser.Lines (parseDouble, parseString, parseStrings)
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

testStr =
  "VIINI: Apothic Dark 2015\n\
  \Maa: Espanja\n\
  \Hinta: 13.49\n\
  \Kuvaus: Pehmeä ja hedelmäinen, täyteläinen\n\
  \SopiiNautittavaksi: seurustelujuomana, pikkusuolaiset, pasta ja pizza, grilliruoka\n\
  \url: https://alko.fi/123"

name :: Parser WineProperty
name = Name <$> parseString "VIINI"

country :: Parser WineProperty
country = Country <$> parseString "Maa"

price :: Parser WineProperty
price = Price <$> parseDouble "Hinta"

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
