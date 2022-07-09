module Parser.Predicates.DigitsSpec where

import Parser.Parser (Parser (runParser))
import Parser.Predicates.Digits
import Test.Hspec

spec :: Spec
spec = do
  describe "Parses a single digit from input" $ do
    it "should return 1 from 123" $ do
      runParser digit "123" `shouldBe` Just (1, "23")
    it "should return Nothing from abc" $ do
      runParser digit "abc" `shouldBe` Nothing

    describe "Parses many digits from input" $ do
      it "should return 123 from 123abc" $ do
        runParser digits "123abc" `shouldBe` Just ([1, 2, 3], "abc")
      it "should return Nothing from abc" $ do
        runParser digits "abc" `shouldBe` Nothing

  describe "Parses an integer from input" $ do
    it "should return 123 from 123abc" $ do
      runParser integer "123abc" `shouldBe` Just (123, "abc")
    it "should return Nothing from abc" $ do
      runParser integer "abc" `shouldBe` Nothing

  describe "Parses a double from input" $ do
    it "should return 123 from 123abc" $ do
      runParser double "123.45abc" `shouldBe` Just (123.45, "abc")
    it "should return Nothing from abc" $ do
      runParser double "abc" `shouldBe` Nothing
