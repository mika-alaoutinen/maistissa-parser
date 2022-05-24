{-# LANGUAGE LambdaCase #-}

module Parser.Predicates where

import Control.Applicative (Alternative (many))
import Data.Char
import Parser.Parser

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
  [] -> Nothing
  x : xs
    | predicate x -> Just (x, xs)
    | otherwise -> Nothing

alphabet :: Parser Char
alphabet = satisfy isAlpha

char :: Char -> Parser Char
char input = satisfy (== input)

digit :: Parser Char
digit = satisfy isDigit

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = many space

string :: String -> Parser String
string = traverse char

newtype KeyValue = KV (String, String) deriving (Show)

withPrefix :: String -> Parser KeyValue
withPrefix prefix = KV <$> parseKv
  where
    parseKv = (,) <$> string prefix <* char ':' <* spaces <*> many alphabet
