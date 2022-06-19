module Parser.Predicates.Chars where

import Data.Char (isDigit, isPrint, isSpace)
import Parser.Parser (Parser, satisfy)

anyChar :: Parser Char
anyChar = satisfy isPrint

char :: Char -> Parser Char
char input = satisfy (== input)

space :: Parser Char
space = satisfy isSpace
