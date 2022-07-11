module Parser.Predicates.DigitsSpec (spec) where

import Parser.Parser (Error (..), Parser (..))
import Parser.Predicates.Digits
import Test.Hspec

invalidInput = "abc"

spec :: Spec
spec = do
  describe "Parses a single digit from input" $ do
    it "should return 1 from 123" $ do
      runParser digit "123" `shouldBe` Right (1, "23")
    it "should return Left from abc" $ do
      runParser digit invalidInput `shouldBe` Left [Unexpected 'a']

  describe "Parses many digits from input" $ do
    it "should return 123 from 123abc" $ do
      runParser digits "123abc" `shouldBe` Right ([1, 2, 3], "abc")
    it "should return Left from abc" $ do
      runParser digits invalidInput `shouldBe` Left [Unexpected 'a']

  describe "Parses an integer from input" $ do
    it "should return 123 from 123abc" $ do
      runParser integer "123abc" `shouldBe` Right (123, "abc")
    it "should return Left from abc" $ do
      runParser integer invalidInput `shouldBe` Left [Unexpected 'a']

  describe "Parses a double from input" $ do
    it "should return 123 from 123abc" $ do
      runParser double "123.45abc" `shouldBe` Right (123.45, "abc")
    it "should return Left from abc" $ do
      runParser double invalidInput `shouldBe` Left [Unexpected 'a']
