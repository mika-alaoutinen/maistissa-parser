{-# LANGUAGE LambdaCase #-}

module Parser.Predicates (newline, withPrefix) where

import Control.Applicative
import Data.Char (isPrint, isSpace)
import Parser.Parser

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
  [] -> Nothing
  x : xs
    | predicate x -> Just (x, xs)
    | otherwise -> Nothing

char :: Char -> Parser Char
char input = satisfy (== input)

string :: String -> Parser String
string = traverse char

-- Char predicates
anyChar :: Parser Char
anyChar = satisfy isPrint

space :: Parser Char
space = satisfy isSpace

stripColon :: Parser Char
stripColon = spaces *> char ':' <* spaces

-- String predicates
anyString :: Parser String
anyString = many anyChar

spaces :: Parser String
spaces = many space

-- Exports
newline :: Parser Char
newline = char '\n' <|> (char '\r' *> char '\n')

withPrefix :: String -> Parser String
withPrefix prefix = string prefix <* stripColon *> anyString
