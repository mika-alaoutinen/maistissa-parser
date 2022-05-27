{-# LANGUAGE LambdaCase #-}

module Parser.Predicates (withPrefix, withPrefix_) where

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

notLineBreak :: Parser Char
notLineBreak = satisfy (/= '\n')

digit :: Parser Char
digit = satisfy isDigit

space :: Parser Char
space = satisfy isSpace

stripColon :: Parser Char
stripColon = spaces *> char ':' <* spaces

-- String predicates
anyString :: Parser String
anyString = many anyChar

untilLineBreak :: Parser String
untilLineBreak = many notLineBreak

spaces :: Parser String
spaces = many space

-- Exports
withPrefix :: String -> Parser KeyValue
withPrefix prefix = KV <$> parseKv
  where
    parseKv = (,) <$> string prefix <* stripColon <*> anyString

withPrefix_ :: String -> Parser String
withPrefix_ prefix = string prefix <* stripColon *> untilLineBreak

testStr = "VIINI: Apothic Dark 2015\nVIINI: Gato Negro"

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- Exports
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy parser separator = sepBy1 parser separator <|> pure []

pKvs :: Parser [KeyValue]
pKvs = withPrefix "VIINI" `sepBy` char ','

names :: Parser [String]
names = withPrefix_ "VIINI" `sepBy` char '\n'
