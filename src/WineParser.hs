{-# LANGUAGE RecordWildCards #-}

module WineParser (parseWine) where

import Model.Wine (Wine (..))
import Parser.Parser (Parser (..))
import Parser.WinePropertyParser (WineProperty (..), winePropertiesParser)

testStr =
  "VIINI: Apothic Dark 2015\n\
  \Maa: Espanja\n\
  \Hinta: 13.49\n\
  \Kuvaus: Pehmeä ja hedelmäinen, täyteläinen\n\
  \SopiiNautittavaksi: seurustelujuomana, pikkusuolaiset, pasta ja pizza, grilliruoka\n\
  \url: https://alko.fi/123"

parseWine :: String -> Maybe Wine
parseWine input = do
  (wineProperties, _) <- runParser winePropertiesParser input
  mkWine wineProperties

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
mkWine _ = Nothing
