module ParserMain where

import Model.Wine (Wine)
import Parser.Parser (Parser (..))
import Parser.WineParser (wineParser, winesParser)

parseWine :: String -> Maybe Wine
parseWine input = case runParser wineParser input of
  Right (wine, _) -> wine
  Left _ -> Nothing

parseWines :: String -> [Wine]
parseWines input = case runParser winesParser input of
  Right (wines, _) -> wines
  Left errors -> []
