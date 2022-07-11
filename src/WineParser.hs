{-# LANGUAGE RecordWildCards #-}

module WineParser (parseWine) where

import Model.Wine (Wine (..))
import Parser.Parser (Error (..), Parser (..))
import Parser.WineProperties (WineProperty (..), winePropertiesParser)

parseWine :: String -> Either [Error] Wine
parseWine input = do
  (wineProperties, _) <- runParser winePropertiesParser input
  mkWine wineProperties

-- Helpers
mkWine :: [WineProperty] -> Either [Error] Wine
mkWine
  [ Name name,
    Country country,
    Price price,
    Description description,
    FoodPairings foodPairings,
    Url url
    ] = Right Wine {..}
mkWine
  [ Name name,
    Country country,
    Price price,
    Description description,
    FoodPairings foodPairings
    ] = Right Wine {url = Nothing, ..}
mkWine _ = Left [Unexpected "p" "placeholder"]
