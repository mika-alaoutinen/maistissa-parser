module Parser.WinePropertiesSpec (spec) where

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

  describe "Parses Nothing on invalid input" $ do
    it "should return Nothing on incorrect prefix" $ do
      parseProp "invalid: Apothic Dark 2015" `shouldBe` Nothing
    it "should return Nothing on empty value" $ do
      parseProp "Maa: " `shouldBe` Nothing

-- Helpers
parseProp :: String -> Maybe (WineProperty, String)
parseProp = runParser winePropertyParser
