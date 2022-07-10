module Parser.Predicates.StringsSpec (spec) where

import Parser.Parser (Parser (runParser))
import Parser.Predicates.Strings
import Test.Hspec

input = "abcdef"

spec :: Spec
spec = do
  describe "Parses a string matching given predicate" $ do
    it "should parse abc from abcdef" $ do
      runParser (string "abc") input `shouldBe` Just ("abc", "def")
    it "should return Nothing when predicate is not found in input" $ do
      runParser (string "x") input `shouldBe` Nothing

  describe "Parses any string input" $ do
    it "should parse entire input" $ do
      runParser anyString input `shouldBe` Just (input, "")
    it "should stop parsing on new line character" $ do
      runParser anyString (input ++ "\nghi") `shouldBe` Just (input, "\nghi")
    it "should return Nothing on empty input" $ do
      runParser anyString "" `shouldBe` Nothing

  describe "Parses zero or more spaces" $ do
    it "should parse 0 spaces" $ do
      runParser spaces input `shouldBe` Just ("", input)
    it "should parse 1 space" $ do
      runParser spaces (" " ++ input) `shouldBe` Just (" ", input)
    it "should parse many spaces" $ do
      runParser spaces ("    " ++ input) `shouldBe` Just ("    ", input)
