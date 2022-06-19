{-# LANGUAGE LambdaCase #-}

module Parser.Parser where

import Control.Applicative (Alternative (empty, (<|>)))

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

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
    runParser (continuation parsed) unparsed

instance Alternative Parser where
  empty = Parser $ const Nothing

  Parser p1 <|> Parser p2 = Parser $ \input -> case p1 input of
    Just result -> Just result
    Nothing -> p2 input

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
  [] -> Nothing
  x : xs
    | predicate x -> Just (x, xs)
    | otherwise -> Nothing