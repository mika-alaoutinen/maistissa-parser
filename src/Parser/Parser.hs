{-# LANGUAGE LambdaCase #-}

module Parser.Parser where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.List (nub)

data Error
  = Empty
  | EndOfInput
  | Unexpected Char String
  deriving (Eq, Show)

newtype Parser a = Parser {runParser :: String -> Either [Error] (a, String)}

instance Functor Parser where
  fmap fn (Parser parser) = Parser $ \input -> do
    (parsed, rest) <- parser input
    pure (fn parsed, rest)

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)

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
  empty = Parser $ \_ -> Left [Empty]

  Parser p1 <|> Parser p2 = Parser $ \input -> case p1 input of
    Left error1 -> case p2 input of
      Left error2 -> Left $ nub $ error1 <> error2
      Right result -> Right result
    Right result -> Right result

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \case
  [] -> Left [EndOfInput]
  x : xs
    | predicate x -> Right (x, xs)
    | otherwise -> Left [Unexpected x (x : xs)]
