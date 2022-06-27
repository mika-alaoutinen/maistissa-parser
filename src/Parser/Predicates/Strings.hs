module Parser.Predicates.Strings where

import Control.Applicative (Alternative (many, some))
import Parser.Parser (Parser)
import Parser.Predicates.Chars (anyChar, char, space)

anyString :: Parser String
anyString = some anyChar

spaces :: Parser String
spaces = many space

string :: String -> Parser String
string = traverse char
