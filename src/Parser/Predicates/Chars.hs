module Parser.Predicates.Chars where

import Control.Applicative ((<|>))
import Data.Char (isDigit, isPrint, isSpace)
import Parser.Parser (Parser, satisfy)

anyChar :: Parser Char
anyChar = satisfy isPrint

char :: Char -> Parser Char
char input = satisfy (== input)

newline :: Parser Char
newline = char '\n' <|> (char '\r' *> char '\n')

space :: Parser Char
space = satisfy isSpace
