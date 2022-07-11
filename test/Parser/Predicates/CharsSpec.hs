module Parser.Predicates.CharsSpec (spec) where

import Parser.Parser (Error (..), Parser (..))
import Parser.Predicates.Chars
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import TestUtils (alphabetString)

input = "abc"

spec :: Spec
spec = do
  describe "Parses a char from input if it matches argument" $ do
    it "should parse 'a' from abc" $ do
      runParser (char 'a') input `shouldBe` Right ('a', "bc")
    it "should return Left on parse fail" $ do
      runParser (char 'x') input `shouldBe` Left [Expected 'x' 'a']

  describe "Parses any char from input" $ do
    it "should parse 'a' from abc" $ do
      runParser anyChar input `shouldBe` Right ('a', "bc")
    it "should return Left on parse fail" $ do
      runParser anyChar "" `shouldBe` Left [EndOfInput]

  describe "Parses a newline character from input" $ do
    it "should parse newline" $ do
      runParser newline "\nabc" `shouldBe` Right ('\n', "abc")
    it "should return Left on parse fail" $ do
      runParser newline input `shouldBe` Left [Unexpected 'a']

  describe "Parses a space from input" $ do
    it "should parse space" $ do
      runParser space " abc" `shouldBe` Right (' ', "abc")
    it "should return Left on parse fail" $ do
      runParser space input `shouldBe` Left [Unexpected 'a']

  describe "QuickCheck test cases" $ do
    prop "any char" prop_anyChar
    prop "a char matching given argument" prop_char
    prop "newline" prop_newline
    prop "space" prop_space

prop_anyChar :: Property
prop_anyChar = forAll alphabetString $ \input -> case parse input of
  Left _ -> False
  Right (parsed, unparsed) -> parsed == head input && unparsed == tail input
  where
    parse = runParser anyChar

prop_char :: Char -> String -> Bool
prop_char c str = case parse input of
  Left _ -> False
  Right (parsed, unparsed) -> parsed == c && unparsed == str
  where
    parse = runParser $ char c
    input = c : str

prop_newline :: Property
prop_newline = forAll input $ \i -> case parse i of
  Left _ -> True
  Right (parsed, unparsed) -> parsed == head i && not (hasLinebreaks unparsed)
  where
    parse = runParser newline
    input = elements ["\r", "\n", "\r\\n", "\r \n", "\r abc"]

prop_space :: Property
prop_space = forAll input $ \i -> case parse i of
  Left _ -> True
  Right (parsed, unparsed) -> unparsed == "def"
  where
    parse = runParser space
    input = elements ["abc", " def"]

-- Helpers
hasLinebreaks :: String -> Bool
hasLinebreaks = any (\c -> c == '\n' || c == '\r')
