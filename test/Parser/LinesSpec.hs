module Parser.LinesSpec where

import Parser.Lines (parseString)
import Parser.Parser (Parser (runParser))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Parse lines starting with given keyword" $ do
    prop "parses a string" prop_parseString

prop_parseString :: String -> Property
prop_parseString prefix = forAll alphabetString $ \str -> case parse (input str) of
  Nothing -> False
  Just (parsed, _) -> parsed == str
  where
    parse = runParser $ parseString prefix
    input value = prefix ++ ": " ++ value

-- Helpers
alphabetString :: Gen String
alphabetString = listOf1 (elements ['a' .. 'z'])
