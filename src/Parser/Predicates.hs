{-# LANGUAGE LambdaCase #-}

module Parser.Predicates where

import Data.Char (isDigit, isPrint, isSpace)
import Parser.Parser (Parser (Parser))

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
  [] -> Nothing
  x : xs
    | predicate x -> Just (x, xs)
    | otherwise -> Nothing

char :: Char -> Parser Char
char input = satisfy (== input)
