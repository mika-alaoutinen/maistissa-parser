module Parser.WineParserSpec where

import Model.Wine (Wine (..))
import Parser.Parser (Error, Parser (..))
import Parser.WineParser (wineParser, winesParser)
import Test.Hspec

spec :: Spec
spec = do
  describe "Parses a single wine" $ do
    it "should parse wine" $ do
      parseOne wineEntry1 `shouldBe` Right (Just expectedWine1, "")
    it "should return Nothing and unparsed string when parsing fails" $ do
      parseOne invalidWineEntry `shouldBe` Right (Nothing, "\nabc")

  describe "Parses multiple wines" $ do
    it "should parse two wines" $ do
      parseMany (wineEntry1 ++ "\n\n" ++ wineEntry2) `shouldBe` Right ([expectedWine1, expectedWine2], "")
    it "should parse first wine and return unparsed string" $ do
      parseMany (wineEntry1 ++ "\n\n" ++ invalidWineEntry) `shouldBe` Right ([expectedWine1], "abc")
    it "should return empty list when no wines were parsed" $ do
      parseMany invalidWineEntry `shouldBe` Right ([], "abc")

-- Helpers
wineEntry1 =
  "VIINI: Apothic Dark 2015\n\
  \Maa: Espanja\n\
  \Hinta: 13.49\n\
  \Kuvaus: Pehmeä ja hedelmäinen, täyteläinen\n\
  \SopiiNautittavaksi: seurustelujuomana, pikkusuolaiset, pasta ja pizza, grilliruoka\n\
  \url: https://alko.fi/123"

wineEntry2 =
  "VIINI: Betola Organic Monastrell 2014\n\
  \Maa: Espanja\n\
  \Hinta: 7.98\n\
  \Kuvaus: Mehevä ja hilloinen, täyteläinen, tanniininen\n\
  \SopiiNautittavaksi: grilliruoka, mausteiset ja lihaisat makkarat, porsas, nauta\n\
  \url: https://alko.fi/321"

invalidWineEntry = "VIINI: Apothic Dark 2015\nabc"

expectedWine1 =
  Wine
    { name = "Apothic Dark 2015",
      country = "Espanja",
      price = 13.49,
      description = ["Pehmeä ja hedelmäinen", "täyteläinen"],
      foodPairings = ["seurustelujuomana", "pikkusuolaiset", "pasta ja pizza", "grilliruoka"],
      url = Just "https://alko.fi/123"
    }

expectedWine2 =
  Wine
    { name = "Betola Organic Monastrell 2014",
      country = "Espanja",
      price = 7.98,
      description = ["Mehevä ja hilloinen", "täyteläinen", "tanniininen"],
      foodPairings = ["grilliruoka", "mausteiset ja lihaisat makkarat", "porsas", "nauta"],
      url = Just "https://alko.fi/321"
    }

parseOne :: String -> Either [Error] (Maybe Wine, String)
parseOne = runParser wineParser

parseMany :: String -> Either [Error] ([Wine], String)
parseMany = runParser winesParser