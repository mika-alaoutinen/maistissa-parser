module Parser.LinesSpec (spec) where

import Data.List (intercalate)
import Parser.Lines (parseDouble, parseString, parseStrings)
import Parser.Parser (Parser (runParser))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import TestUtils (alphabetString)

spec :: Spec
spec = do
  describe "Parse lines starting with given keyword" $ do
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
