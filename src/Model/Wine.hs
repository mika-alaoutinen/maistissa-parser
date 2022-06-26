module Model.Wine where

data Wine = Wine
  { name :: String,
    country :: String,
    price :: Double,
    description :: [String],
    foodPairings :: [String],
    url :: Maybe String
  }
  deriving (Show, Eq)
