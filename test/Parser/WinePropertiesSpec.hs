module Parser.WinePropertiesSpec (spec) where

import Data.Maybe (fromMaybe)
import Parser.Parser (Parser (runParser))
import Parser.WineProperties
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Parses a single wine property" $ do
    it "should parse name" $ do
      parseProp "VIINI: Apothic Dark 2015" `shouldBe` Just (Name "Apothic Dark 2015", "")
    it "should parse country" $ do
      parseProp "Maa: Spain" `shouldBe` Just (Country "Spain", "")
    it "should parse price" $ do
      parseProp "Hinta: 13" `shouldBe` Just (Price 13.0, "")
    it "should parse description" $ do
      parseProp "Kuvaus: makea, täyteläinen" `shouldBe` Just (Description ["makea", "täyteläinen"], "")
    it "should parse food pairings" $ do
      parseProp "SopiiNautittavaksi: kana, juustot" `shouldBe` Just (FoodPairings ["kana", "juustot"], "")
    it "should parse URL" $ do
      parseProp "url: http://viini.fi/123" `shouldBe` Just (Url $ Just "http://viini.fi/123", "")

  describe "Parses many wine properties that are separeted by newline" $ do
    it "should parse all wine properties" $ do
      fromMaybe 0 parsedPropertiesCount `shouldBe` 6
    it "should parse wine name and stop on invalid line" $ do
      parseProps invalidWineEntry `shouldBe` Just ([Name "Apothic Dark 2015"], "\nabc")

  describe "Parses Nothing on invalid input" $ do
    it "should return Nothing on incorrect prefix" $ do
      parseProp "invalid: Apothic Dark 2015" `shouldBe` Nothing
    it "should return Nothing on empty value" $ do
      parseProp "Maa: " `shouldBe` Nothing

-- Helpers
wineEntry =
  "VIINI: Apothic Dark 2015\n\
  \Maa: Espanja\n\
  \Hinta: 13.49\n\
  \Kuvaus: Pehmeä ja hedelmäinen, täyteläinen\n\
  \SopiiNautittavaksi: seurustelujuomana, pikkusuolaiset, pasta ja pizza, grilliruoka\n\
  \url: https://alko.fi/123"

invalidWineEntry = "VIINI: Apothic Dark 2015\nabc"

parseProp :: String -> Maybe (WineProperty, String)
parseProp = runParser winePropertyParser

parseProps :: String -> Maybe ([WineProperty], String)
parseProps = runParser winePropertiesParser

parsedPropertiesCount :: Maybe Int
parsedPropertiesCount = do
  (parsedProperties, _) <- parseProps wineEntry
  return $ length parsedProperties