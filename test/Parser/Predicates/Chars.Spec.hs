import Parser.Parser (Parser (runParser))
import Parser.Predicates.Chars
import Test.QuickCheck

runTests :: IO ()
runTests = do
  quickCheck prop_anyChar
  quickCheck prop_char
  quickCheck prop_newline
  quickCheck prop_space

prop_anyChar :: Property
prop_anyChar = forAll alphabetString $ \input -> case parse input of
  Nothing -> False
  Just (parsed, unparsed) -> parsed == head input && unparsed == tail input
  where
    parse = runParser anyChar

prop_char :: Char -> String -> Bool
prop_char c str = case parse input of
  Nothing -> False
  Just (parsed, unparsed) -> parsed == c && unparsed == str
  where
    parse = runParser $ char c
    input = c : str

prop_newline :: Property
prop_newline = forAll input $ \i -> case parse i of
  Nothing -> True
  Just (parsed, unparsed) -> parsed == head i && not (hasLinebreaks unparsed)
  where
    parse = runParser newline
    input = elements ["\r", "\n", "\r\\n", "\r \n", "\r abc"]

prop_space :: Property
prop_space = forAll input $ \i -> case parse i of
  Nothing -> True
  Just (parsed, unparsed) -> unparsed == "abc"
  where
    parse = runParser space
    input = elements ["abc", " abc"]

-- Helpers
alphabetString :: Gen String
alphabetString = listOf1 (elements ['a' .. 'z'])

hasLinebreaks :: String -> Bool
hasLinebreaks = any (\c -> c == '\n' || c == '\r')
