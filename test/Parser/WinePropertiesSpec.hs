module Parser.WinePropertiesSpec (spec) where

import Parser.Parser (Parser (runParser))
import Parser.WineProperties
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Parses wine name" $ do
    it "should parse wine name" $ do
      parseProp "VIINI: Apothic Dark 2015" `shouldBe` Just (Name "Apothic Dark 2015", "")
    it "should return Nothing on incorrect prefix" $ do
      parseProp "invalid: Apothic Dark 2015" `shouldBe` Nothing
    it "should return Nothing on empty value" $ do
      parseProp "VIINI: " `shouldBe` Nothing

  describe "Parses wine country" $ do
    it "should parse wine country" $ do
      parseProp "Maa: Spain" `shouldBe` Just (Country "Spain", "")
    it "should return Nothing on incorrect prefix" $ do
      parseProp "invalid: Spain" `shouldBe` Nothing
    it "should return Nothing on empty value" $ do
      parseProp "Maa: " `shouldBe` Nothing

  describe "Parses wine price" $ do
    it "should parse wine price" $ do
      parseProp "Hinta: 13" `shouldBe` Just (Price 13.0, "")

-- Helpers
parseProp :: String -> Maybe (WineProperty, String)
parseProp = runParser winePropertyParser
