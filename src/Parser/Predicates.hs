{-# LANGUAGE LambdaCase #-}

module Parser.Predicates (KeyValue, newline, withPrefix, withPrefix_) where

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

withPrefix :: String -> Parser KeyValue
withPrefix prefix = KV <$> parseKv
  where
    parseKv = (,) <$> string prefix <* stripColon <*> anyString

withPrefix_ :: String -> Parser String
withPrefix_ prefix = string prefix <* stripColon *> anyString
