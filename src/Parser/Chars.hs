module Parser.Chars where

import Data.Char (isDigit, isPrint, isSpace)
import Parser.Parser (Parser)
import Parser.Predicates (satisfy)

anyChar :: Parser Char
anyChar = satisfy isPrint

space :: Parser Char
space = satisfy isSpace
