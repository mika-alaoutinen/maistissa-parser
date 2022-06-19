module Parser.Predicates.Digits where

import Control.Applicative (Alternative (..))
import Data.Char (digitToInt, isDigit)
import Parser.Parser (Parser (..), satisfy)
import Parser.Predicates.Chars (char)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

digits :: Parser [Int]
digits = many digit

integer :: Parser Int
integer = fromDigits <$> digits

anyDecimal :: Parser [Int]
anyDecimal = digits <* decimalPoint <* digits
  where
    decimalPoint = char '.' <|> char ','

-- Helpers
fractional :: Parser [Int]
fractional = decimalPoint *> digits
  where
    decimalPoint = char '.' <|> char ','

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
  where
    addDigit num d = 10 * num + d