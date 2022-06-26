module WineParser where

import Control.Applicative (Alternative ((<|>)))
import Model.Wine (Wine (..))
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
  \SopiiNautittavaksi: seurustelujuomana, pikkusuolaiset, pasta ja pizza, grilliruoka\n"

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

urlParser :: Parser WineProperty
urlParser = Url . Just <$> parseString "url"

-- Parse wine properties
winePropertyParser :: Parser WineProperty
winePropertyParser =
  nameParser
    <|> countryParser
    <|> priceParser
    <|> descriptionParser
    <|> foodPairingsParser
    <|> urlParser

winePropertiesParser :: Parser [WineProperty]
winePropertiesParser = winePropertyParser `separatedBy` newline

parseWine :: String -> Maybe Wine
parseWine input = do
  (Name name, unparsed1) <- runParser winePropertyParser input
  (Country country, unparsed2) <- runParser winePropertyParser unparsed1
  (Price price, unparsed3) <- runParser winePropertyParser unparsed2
  (Description description, unparsed4) <- runParser winePropertyParser unparsed3
  (FoodPairings foodPairings, unparsed5) <- runParser winePropertyParser unparsed4
  (Url url, unparsed6) <- runParser winePropertyParser unparsed5

  Just
    Wine
      { name = name,
        country = country,
        price = price,
        description = description,
        foodPairings = foodPairings,
        url = url
      }