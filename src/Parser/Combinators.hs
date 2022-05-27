module Parser.Combinators where

import Control.Applicative
import Parser.Parser (Parser (..))
import Parser.Predicates

testStr = "VIINI: Apothic Dark 2015\nMaa: Espanja"

data Wine = Name String | Country String deriving (Show, Eq)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 parser separator = fmap (:) parser <*> parsers
  where
    parsers = many (separator *> parser)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy parser separator = sepBy1 parser separator <|> pure []

pKvs :: Parser [KeyValue]
pKvs = withPrefix "VIINI" `sepBy` newline

names :: Parser [String]
names = withPrefix_ "VIINI" `sepBy` newline

nameParser :: Parser String
nameParser = withPrefix_ "VIINI"

countryParser :: Parser String
countryParser = withPrefix_ "Maa"

wineParser :: Parser [String]
wineParser = parser `sepBy` newline
  where
    parser = nameParser <|> countryParser
