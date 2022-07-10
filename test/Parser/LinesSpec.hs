module Parser.LinesSpec (spec) where

import Data.List (intercalate)
import Parser.Lines
import Parser.Parser (Error (..), Parser (..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TestUtils (alphabetString)

prefix = "key"

input = inputValue "123.45 abc"

spec :: Spec
spec = do
  describe "Parses a number starting with given prefix" $ do
    it "should parse a decimal number with given prefix" $ do
      runParser (parseNumber prefix) input `shouldBe` Right (123.45, " abc")
    it "should parse an integer with given prefix and convert it to double" $ do
      runParser (parseNumber prefix) (inputValue "123 abc") `shouldBe` Right (123.0, " abc")
    it "should fail on empty input" $ do
      runParser (parseNumber prefix) "" `shouldBe` Left [EndOfInput]

  describe "Parses a string starting with given prefix" $ do
    it "should parse a string with given prefix" $ do
      runParser (parseString prefix) input `shouldBe` Right ("123.45 abc", "")
    it "should parse until line breek" $ do
      runParser (parseString prefix) (input ++ "\ndef") `shouldBe` Right ("123.45 abc", "\ndef")
    it "should fail on empty input" $ do
      runParser (parseString prefix) "" `shouldBe` Left [EndOfInput]

  describe "Parses a string of comma separated values into list" $ do
    it "should parse aa, bb, cc" $ do
      runParser (parseStrings prefix) (inputValue "aa, bb, cc") `shouldBe` Right (["aa", "bb", "cc"], "")
    it "should return singleton list on input without commas" $ do
      runParser (parseStrings prefix) input `shouldBe` Right (["123.45 abc"], "")
    it "should fail on empty input" $ do
      runParser (parseStrings prefix) "" `shouldBe` Left [EndOfInput]

  describe "QuickCheck test cases" $ do
    prop "parses a double" prop_parseNumber
    prop "parses a string" prop_parseString
    prop "parses a comma separated string" prop_parseStrings

prop_parseNumber :: String -> Property
prop_parseNumber prefix = forAll alphabetString $ \str -> case parse (input str) of
  Left _ -> True
  Right (parsed, _) -> parsed == read str
  where
    parse = runParser $ parseNumber prefix
    input value = prefix ++ ": " ++ value

prop_parseString :: String -> Property
prop_parseString prefix = forAll alphabetString $ \str -> case parse (input str) of
  Left _ -> False
  Right (parsed, _) -> parsed == str
  where
    parse = runParser $ parseString prefix
    input value = prefix ++ ": " ++ value

prop_parseStrings :: String -> Property
prop_parseStrings prefix = forAll alphabetString $ \str -> case parse (input str) of
  Left _ -> False
  Right (parsed, _) -> intercalate ", " parsed == str
  where
    parse = runParser $ parseStrings prefix
    input value = prefix ++ ": " ++ value

-- Helpers
inputValue :: String -> String
inputValue value = prefix ++ ": " ++ value