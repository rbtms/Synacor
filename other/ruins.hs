module Ruins where

import Data.List (permutations, find)

is_valid :: [Int] -> Bool
is_valid [a, b, c, d, e] = a + b * c^2 + d^3 - e == 399

main :: IO ()
main = do
  let pos = permutations input
  let res = find is_valid pos
  print res
  where input = [2, 3, 5, 7, 9]
