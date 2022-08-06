{-# LANGUAGE RecordWildCards #-}

module Parser.WineParser (wineParser, winesParser) where

import Data.Maybe (catMaybes)
import Model.Wine (Wine (..))
import Parser.Combinators (separatedBy)
import Parser.Parser (Parser (..))
import Parser.Predicates.Chars (newline)
import Parser.WineProperties (WineProperty (..), winePropertiesParser)

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
