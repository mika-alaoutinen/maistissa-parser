{-# LANGUAGE LambdaCase #-}

-- Based on:
-- https://serokell.io/blog/parser-combinators-in-haskell
-- https://github.com/japiirainen/microparser/blob/main/src/MicroParser.hs

module Parser (Parser, parse, char, isAlphabet, isSpace, string) where

import qualified Data.Char as Char

newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap fn (Parser parser) = Parser $ \input -> do
    (parsed, unparsed) <- parser input
    pure (fn parsed, unparsed)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (a, input)

  Parser p1 <*> Parser p2 = Parser $ \input -> do
    (p1', unparsed) <- p1 input
    (p2', unparsed') <- p2 unparsed
    pure (p1' p2', unparsed')

instance Monad Parser where
  return = pure

  Parser parser >>= continuation = Parser $ \input -> do
    (parsed, unparsed) <- parser input
    parse (continuation parsed) unparsed

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

isSpace :: Parser Char
isSpace = satisfy Char.isSpace
