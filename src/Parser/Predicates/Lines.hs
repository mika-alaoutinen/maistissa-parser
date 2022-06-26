module Parser.Predicates.Lines (newline, parseDouble, parseString, parseStrings) where

import Control.Applicative ((<|>))
import Data.List.Split (splitOn)
import Parser.Parser (Parser)
import Parser.Predicates.Chars (char)
import Parser.Predicates.Digits (double)
import Parser.Predicates.Strings (anyString, spaces, string)

newline :: Parser Char
newline = char '\n' <|> (char '\r' *> char '\n')

parseDouble :: String -> Parser Double
parseDouble prefix = withPrefix prefix double

parseString :: String -> Parser String
parseString prefix = withPrefix prefix anyString

parseStrings :: String -> Parser [String]
parseStrings prefix = withPrefix prefix $ commaSeparated <$> anyString

-- Helpers
commaSeparated :: String -> [String]
commaSeparated = splitOn ", "

stripColon :: Parser Char
stripColon = spaces *> char ':' <* spaces

withPrefix :: String -> Parser a -> Parser a
withPrefix prefix predicate = string prefix <* stripColon *> predicate