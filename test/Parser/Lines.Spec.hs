module Parser.Lines.Spec where

import Parser.Lines (parseString)
import Parser.Parser (Parser (runParser))
import Test.QuickCheck

runTests :: IO ()
runTests = do
  quickCheck prop_parseString

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
