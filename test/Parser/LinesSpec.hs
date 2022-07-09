module Parser.LinesSpec (spec) where

import Data.List (intercalate)
import Parser.Lines (parseDouble, parseString, parseStrings)
import Parser.Parser (Parser (runParser))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TestUtils (alphabetString)

prefix = "key"

input = inputValue "123.45 abc"

spec :: Spec
spec = do
  describe "Parses a line starting with given prefix" $ do
    it "should parse a double with given prefix" $ do
      runParser (parseDouble prefix) input `shouldBe` Just (123.45, " abc")
    it "should fail if input is not a decimal number" $ do
      runParser (parseDouble prefix) (inputValue "123") `shouldBe` Nothing
    it "should fail on empty input" $ do
      runParser (parseDouble prefix) "" `shouldBe` Nothing

  describe "Parses a string starting with given prefix" $ do
    it "should parse a string with given prefix" $ do
      runParser (parseString prefix) input `shouldBe` Just ("123.45 abc", "")
    it "should parse until line breek" $ do
      runParser (parseString prefix) (input ++ "\ndef") `shouldBe` Just ("123.45 abc", "\ndef")
    it "should fail on empty input" $ do
      runParser (parseString prefix) "" `shouldBe` Nothing

  describe "Parses a string of comma separated values into list" $ do
    it "should parse aa, bb, cc" $ do
      runParser (parseStrings prefix) (inputValue "aa, bb, cc") `shouldBe` Just (["aa", "bb", "cc"], "")
    it "should return singleton list on input without commas" $ do
      runParser (parseStrings prefix) input `shouldBe` Just (["123.45 abc"], "")
    it "should fail on empty input" $ do
      runParser (parseStrings prefix) "" `shouldBe` Nothing

  describe "QuickCheck test cases" $ do
    prop "parses a double" prop_parseDouble
    prop "parses a string" prop_parseString
    prop "parses a comma separated string" prop_parseStrings

prop_parseDouble :: String -> Property
prop_parseDouble prefix = forAll alphabetString $ \str -> case parse (input str) of
  Nothing -> True
  Just (parsed, _) -> parsed == read str
  where
    parse = runParser $ parseDouble prefix
    input value = prefix ++ ": " ++ value

prop_parseString :: String -> Property
prop_parseString prefix = forAll alphabetString $ \str -> case parse (input str) of
  Nothing -> False
  Just (parsed, _) -> parsed == str
  where
    parse = runParser $ parseString prefix
    input value = prefix ++ ": " ++ value

prop_parseStrings :: String -> Property
prop_parseStrings prefix = forAll alphabetString $ \str -> case parse (input str) of
  Nothing -> False
  Just (parsed, _) -> intercalate ", " parsed == str
  where
    parse = runParser $ parseStrings prefix
    input value = prefix ++ ": " ++ value

-- Helpers
inputValue :: String -> String
inputValue value = prefix ++ ": " ++ value