module Parser.Strings where

import Control.Applicative (Alternative (many))
import Parser.Chars (anyChar, space)
import Parser.Parser (Parser)
import Parser.Predicates (char)

anyString :: Parser String
anyString = many anyChar

spaces :: Parser String
spaces = many space

string :: String -> Parser String
string = traverse char
