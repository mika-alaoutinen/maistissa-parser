module Parser.Predicates.Chars where

import Control.Applicative ((<|>))
import Data.Char (isDigit, isPrint, isSpace)
import Parser.Parser (Error (..), Parser, satisfy)

anyChar :: Parser Char
anyChar = satisfy isPrint NotFound

char :: Char -> Parser Char
char c = charParser c (Unexpected c)

newline :: Parser Char
newline = unixNewline <|> windowsNewline
  where
    unixNewline = charParser '\n' NotFound
    windowsNewline = charParser '\r' NotFound *> charParser '\n' NotFound

space :: Parser Char
space = satisfy isSpace NotFound

-- Helpers
charParser :: Char -> (String -> Error) -> Parser Char
charParser c = satisfy (== c)