module Parser.Predicates.Digits where

import Control.Applicative (Alternative (..))
import Data.Char (digitToInt, isDigit)
import Parser.Parser (Parser, satisfy)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

digits :: Parser [Int]
digits = many digit

-- anyDecimal :: Parser String
-- anyDecimal = digits <* decimalPoint <* digits
--   where
--     decimalPoint = char '.' <|> char ','
