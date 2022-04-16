module Golf where

import Data.Bool (bool)
import Data.List (elemIndices)

skips :: [a] -> [[a]]
skips xs = [[x | (i, x) <- zip [1 ..] xs, i `mod` n == 0] | n <- [1 .. length xs]]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = [n2 | (n1, n2, n3) <- zip3 xs (tail xs) (drop 2 xs), n2 > n1 && n2 > n3]

histogram :: [Integer] -> String
histogram xs = unlines $ [bool ' ' '*' . (>= n) <$> count | n <- [maxh, maxh - 1 .. 1]] ++ ["==========", "0123456789"]
  where
    count = [length $ elemIndices n xs | n <- [0 .. 9]]
    maxh = maximum count