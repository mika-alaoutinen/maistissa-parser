module Parser.Predicates.Chars (anyChar, char, newline, space) where

import Control.Applicative ((<|>))
import Data.Char (isDigit, isPrint, isSpace)
import Parser.Parser (Error (..), Parser, satisfy)

anyChar :: Parser Char
anyChar = satisfy isPrint Unexpected

char :: Char -> Parser Char
char c = charParser c (Expected c)

newline :: Parser Char
newline = unixNewline <|> windowsNewline
  where
    unixNewline = charParser '\n' Unexpected
    windowsNewline = charParser '\r' Unexpected *> charParser '\n' Unexpected

space :: Parser Char
space = satisfy isSpace Unexpected

-- Helpers
charParser :: Char -> (Char -> Error) -> Parser Char
charParser c = satisfy (== c)
