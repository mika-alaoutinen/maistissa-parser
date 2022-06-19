module Parser.Digits where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit)
import Parser.Parser (Parser)
import Parser.Predicates

digit :: Parser Char
digit = satisfy isDigit

digits :: Parser String
digits = many digit

anyDecimal :: Parser String
anyDecimal = digits <* decimalPoint <* digits
  where
    decimalPoint = char '.' <|> char ','
