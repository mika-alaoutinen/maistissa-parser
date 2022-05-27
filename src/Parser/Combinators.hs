module Parser.Combinators where

import Control.Applicative
import Parser.Parser (Parser (..))
import Parser.Predicates

testStr = "VIINI: Apothic Dark 2015\nMaa: Espanja"

data WineProp = Name String | Country String deriving (Show, Eq)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy parser separator = (:) <$> parser <*> many (separator *> parser)

separatedBy :: Parser a -> Parser b -> Parser [a]
separatedBy parser separator = sepBy parser separator <|> pure []

names :: Parser [String]
names = withPrefix "VIINI" `separatedBy` newline

nameParser :: Parser WineProp
nameParser = Name <$> withPrefix "VIINI"

countryParser :: Parser WineProp
countryParser = Country <$> withPrefix "Maa"

wineParser :: Parser WineProp
wineParser = nameParser <|> countryParser
