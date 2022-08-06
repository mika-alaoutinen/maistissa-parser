{-# LANGUAGE RecordWildCards #-}

module WineParser where

import Data.Maybe (catMaybes)
import Model.Wine (Wine (..))
import Parser.Combinators (separatedBy)
import Parser.Parser (Parser (..))
import Parser.Predicates.Chars (newline)
import Parser.WineProperties (WineProperty (..), winePropertiesParser)

parseWine :: String -> Maybe Wine
parseWine input = case runParser winePropertiesParser input of
  Right (wineProperties, _) -> mkWine wineProperties
  Left _ -> Nothing

parseWines :: String -> [Wine]
parseWines input = case runParser winesParser input of
  Right (wines, _) -> wines
  Left errors -> []

wineParser :: Parser (Maybe Wine)
wineParser = mkWine <$> winePropertiesParser

winesParser :: Parser [Wine]
winesParser = catMaybes <$> wineParser `separatedBy` newline

-- Helpers
mkWine :: [WineProperty] -> Maybe Wine
mkWine
  [ Name name,
    Country country,
    Price price,
    Description description,
    FoodPairings foodPairings,
    Url url
    ] = Just Wine {..}
mkWine
  [ Name name,
    Country country,
    Price price,
    Description description,
    FoodPairings foodPairings
    ] = Just Wine {url = Nothing, ..}
mkWine _ = Nothing
