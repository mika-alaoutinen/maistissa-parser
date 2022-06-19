module Parser.Predicates.Digits (digit, digits, double, integer) where

import Control.Applicative (Alternative (..))
import Data.Char (digitToInt, isDigit)
import Parser.Parser (Parser (..), satisfy)
import Parser.Predicates.Chars (char)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

digits :: Parser [Int]
digits = many digit

double :: Parser Double
double = toDouble <$> integer <*> fractional

integer :: Parser Int
integer = fromDigits <$> digits

-- Helpers
fractional :: Parser Int
fractional = decimalPoint *> integer
  where
    decimalPoint = char '.' <|> char ','

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
  where
    addDigit num d = 10 * num + d

toDouble :: Int -> Int -> Double
toDouble wholeNum decimal = fromIntegral wholeNum + (fromIntegral decimal / 100)