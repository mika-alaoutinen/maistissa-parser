{-# LANGUAGE LambdaCase #-}

module Parser.Predicates where

import Control.Applicative
import Data.Char
import Parser.Parser

newtype KeyValue = KV (String, String) deriving (Show)

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
alphabet :: Parser Char
alphabet = satisfy isAlpha

anyChar :: Parser Char
anyChar = satisfy isAscii

digit :: Parser Char
digit = satisfy isDigit

space :: Parser Char
space = satisfy isSpace

-- String predicates
anyString :: Parser String
anyString = many anyChar

spaces :: Parser String
spaces = many space

-- More complex predicates
withPrefix :: String -> Parser KeyValue
withPrefix prefix = KV <$> parseKv
  where
    parseKv = (,) <$> string prefix <* stripColon <*> anyString

withPrefix_ :: String -> Parser String
withPrefix_ prefix = string prefix <* stripColon *> anyString

-- Helper functions
stripColon :: Parser Char
stripColon = spaces *> char ':' <* spaces
