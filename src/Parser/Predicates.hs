{-# LANGUAGE LambdaCase #-}

module Parser.Predicates (newline, withPrefix) where

import Control.Applicative
import Data.Char (isDigit, isPrint, isSpace)
import Parser.Parser

-- Primitive predicates
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
  [] -> Nothing
  x : xs
    | predicate x -> Just (x, xs)
    | otherwise -> Nothing

char :: Char -> Parser Char
char input = satisfy (== input)

digit :: Parser Char
digit = satisfy isDigit

string :: String -> Parser String
string = traverse char

-- Char predicates
anyChar :: Parser Char
anyChar = satisfy isPrint

space :: Parser Char
space = satisfy isSpace

-- String predicates
anyString :: Parser String
anyString = many anyChar

spaces :: Parser String
spaces = many space

-- Number predicates
digits :: Parser String
digits = many digit

anyDecimal :: Parser String
anyDecimal = digits <* decimalPoint <* digits
  where
    decimalPoint = char '.' <|> char ','

-- Helpers
stripColon :: Parser Char
stripColon = spaces *> char ':' <* spaces

-- Exports
newline :: Parser Char
newline = char '\n' <|> (char '\r' *> char '\n')

withPrefix :: String -> Parser String
withPrefix prefix = string prefix <* stripColon *> anyString
