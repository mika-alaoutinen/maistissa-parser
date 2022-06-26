module WineParser where

import Control.Applicative (Alternative ((<|>)))
import Model.Wine (Wine (..))
import Parser.Combinators (separatedBy)
import Parser.Lines (parseDouble, parseString, parseStrings)
import Parser.Parser (Parser (..))
import Parser.Predicates.Chars (newline)
import Debug.Trace
import Data.Maybe (fromMaybe)

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

-- Parsinta epäonnistuu, koska viinin nimen jälkeen unparsed1-merkkijonon alussa on rivinvaihto.
-- Rivinvaihto täytyy joko poistaa tai keksiä jokin tapa parsia jokainen rivi `separatedBy` \n.
parseWine :: String -> Maybe Wine
parseWine input = do
  (Name name, unparsed1) <- runParser winePropertyParser (dropLineBreak input)
  (Country country, unparsed2) <- runParser winePropertyParser (dropLineBreak unparsed1)
  (Price price, unparsed3) <- runParser winePropertyParser (dropLineBreak unparsed2)
  (Description description, unparsed4) <- runParser winePropertyParser (dropLineBreak unparsed3)
  (FoodPairings foodPairings, unparsed5) <- runParser winePropertyParser (dropLineBreak unparsed4)
  (Url url, unparsed6) <- runParser winePropertyParser (dropLineBreak unparsed5)

  trace ("Name: " ++ name) Just name
  trace ("Country: " ++ country) Just country
  trace ("Price: " ++ show price) Just price
  trace ("Description: " ++ show (length description)) Just description
  trace ("Food Pairings: " ++ show (length foodPairings)) Just foodPairings
  trace ("url: " ++ fromMaybe "no url" url) url

  Just
    Wine
      { name = name,
        country = country,
        price = price,
        description = description,
        foodPairings = foodPairings,
        url = url
      }

dropLineBreak :: String -> String
dropLineBreak = dropWhile (== '\n')