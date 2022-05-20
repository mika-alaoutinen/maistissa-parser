{-# LANGUAGE LambdaCase #-}

module Parser.Predicates where

import qualified Data.Char as Char
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

isAlphabet :: Parser Char
isAlphabet = satisfy Char.isAlpha

isDigit :: Parser Char
isDigit = satisfy Char.isDigit

isSpace :: Parser Char
isSpace = satisfy Char.isSpace
