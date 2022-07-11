{-# LANGUAGE RecordWildCards #-}

module WineParser (parseWine) where

import Model.Wine (Wine (..))
import Parser.Parser (Error (..), Parser (..))
import Parser.WineProperties (WineProperty (..), winePropertiesParser)

parseWine :: String -> Maybe Wine
parseWine input = case runParser winePropertiesParser input of
  Right (wineProperties, _) -> mkWine wineProperties
  Left _ -> Nothing

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
