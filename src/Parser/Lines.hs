module Parser.Lines (parseNumber, parseString, parseStrings) where

import Control.Applicative ((<|>))
import Data.List.Split (splitOn)
import Parser.Parser (Parser)
import Parser.Predicates.Chars (char)
import Parser.Predicates.Digits (double, integer)
import Parser.Predicates.Strings (anyString, spaces, string)

parseNumber :: String -> Parser Double
parseNumber prefix = doubleParser <|> integerParser
  where
    doubleParser = parseDouble prefix
    integerParser = fromIntegral <$> parseInteger prefix

parseString :: String -> Parser String
parseString prefix = withPrefix prefix anyString

parseStrings :: String -> Parser [String]
parseStrings prefix = withPrefix prefix $ commaSeparated <$> anyString

-- Helpers
commaSeparated :: String -> [String]
commaSeparated = splitOn ", "

parseDouble :: String -> Parser Double
parseDouble prefix = withPrefix prefix double

parseInteger :: String -> Parser Int
parseInteger prefix = withPrefix prefix integer

stripColon :: Parser Char
stripColon = spaces *> char ':' <* spaces

withPrefix :: String -> Parser a -> Parser a
withPrefix prefix predicate = string prefix <* stripColon *> predicate