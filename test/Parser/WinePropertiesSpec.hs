module Parser.WinePropertiesSpec (spec) where

import Data.Either (fromRight)
import Parser.Parser (Error (..), Parser (..))
import Parser.WineProperties
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Parses a single wine property" $ do
    it "should parse name" $ do
      parseProp "VIINI: Apothic Dark 2015" `shouldBe` Right (Name "Apothic Dark 2015", "")
    it "should parse country" $ do
      parseProp "Maa: Spain" `shouldBe` Right (Country "Spain", "")
    it "should parse price" $ do
      parseProp "Hinta: 13" `shouldBe` Right (Price 13.0, "")
    it "should parse description" $ do
      parseProp "Kuvaus: makea, täyteläinen" `shouldBe` Right (Description ["makea", "täyteläinen"], "")
    it "should parse food pairings" $ do
      parseProp "SopiiNautittavaksi: kana, juustot" `shouldBe` Right (FoodPairings ["kana", "juustot"], "")
    it "should parse URL" $ do
      parseProp "url: http://viini.fi/123" `shouldBe` Right (Url $ Just "http://viini.fi/123", "")

  describe "Parses many wine properties that are separeted by newline" $ do
    it "should parse all wine properties" $ do
      fromRight 0 parsedPropertiesCount `shouldBe` 6
    it "should parse wine name and stop on invalid line" $ do
      parseProps invalidWineEntry `shouldBe` Right ([Name "Apothic Dark 2015"], "\nabc")

  describe "Parses Nothing on invalid input" $ do
    it "should return Nothing on incorrect prefix" $ do
      parseProp "invalid: Apothic Dark 2015" `shouldBe` Left [Unexpected "i" "invalid: Apothic Dark 2015"]
    it "should return Nothing on empty value" $ do
      parseProp "Maa: " `shouldBe` Left [EndOfInput]

-- Helpers
wineEntry =
  "VIINI: Apothic Dark 2015\n\
  \Maa: Espanja\n\
  \Hinta: 13.49\n\
  \Kuvaus: Pehmeä ja hedelmäinen, täyteläinen\n\
  \SopiiNautittavaksi: seurustelujuomana, pikkusuolaiset, pasta ja pizza, grilliruoka\n\
  \url: https://alko.fi/123"

invalidWineEntry = "VIINI: Apothic Dark 2015\nabc"

parseProp :: String -> Either [Error] (WineProperty, String)
parseProp = runParser winePropertyParser

parseProps :: String -> Either [Error] ([WineProperty], String)
parseProps = runParser winePropertiesParser

parsedPropertiesCount :: Either [Error] Int
parsedPropertiesCount = do
  (parsedProperties, _) <- parseProps wineEntry
  return $ length parsedProperties
