module WineParser where

import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (fromMaybe)
import Debug.Trace
import Model.Wine (Wine (..))
import Parser.Combinators (separatedBy)
import Parser.Lines (parseDouble, parseString, parseStrings)
import Parser.Parser (Parser (..))
import Parser.Predicates.Chars (newline)
import Parser.WinePropertyParser

parseWine1 :: String -> Maybe Wine
parseWine1 input = do
  (wineProperties, _) <- runParser winePropertiesParser input
  wineFromProperties wineProperties

wineFromProperties :: [WineProperty] -> Maybe Wine
wineFromProperties properties = Nothing

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