module Parser.Predicates.Strings where

import Control.Applicative (Alternative (many))
import Parser.Parser (Parser)
import Parser.Predicates.Chars (anyChar, space)
import Parser.Predicates.Primitives (char)

anyString :: Parser String
anyString = many anyChar

spaces :: Parser String
spaces = many space

string :: String -> Parser String
string = traverse char
