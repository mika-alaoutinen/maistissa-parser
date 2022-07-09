module TestUtils where

import Test.QuickCheck (Gen, elements, listOf1)

alphabetString :: Gen String
alphabetString = listOf1 (elements ['a' .. 'z'])
