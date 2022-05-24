module Wine where

data Wine = Wine
  { name :: String,
    country :: String,
    description :: [String],
    foodPairings :: [String]
  }
  deriving (Show, Eq)
