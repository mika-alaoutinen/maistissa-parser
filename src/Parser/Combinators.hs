module Parser.Combinators (separatedBy) where

import Control.Applicative
import Parser.Parser

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy parser separator = (:) <$> parser <*> many (separator *> parser)

separatedBy :: Parser a -> Parser b -> Parser [a]
separatedBy parser separator = sepBy parser separator <|> pure []
