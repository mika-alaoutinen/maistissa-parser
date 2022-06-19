module Parser.Lines (newline, withPrefix) where

import Control.Applicative ((<|>))
import Parser.Parser (Parser)
import Parser.Predicates (char)
import Parser.Strings (anyString, spaces, string)

newline :: Parser Char
newline = char '\n' <|> (char '\r' *> char '\n')

withPrefix :: String -> Parser String
withPrefix prefix = string prefix <* stripColon *> anyString

-- Helpers
stripColon :: Parser Char
stripColon = spaces *> char ':' <* spaces