{-# HLINT ignore "Use map" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import qualified Data.Bits
import Data.Bool (bool)
import Data.List ((\\))

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate (\t -> if even t then t `div` 2 else 3 * t + 1)

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert :: (Ord a) => a -> Tree a -> Tree a
    insert v Leaf = Node 0 Leaf v Leaf
    insert v1 (Node h t1 v2 t2)
      | v1 < v2 = rotate $ Node h (insert v1 t1) v2 t2
      | otherwise = rotate $ Node h t1 v2 (insert v1 t2)
    rotate :: (Ord a) => Tree a -> Tree a
    rotate Leaf = Leaf
    rotate (Node h l v r)
      | not (balanced l) = Node h (rotate l) v r
      | not (balanced r) = Node h l v (rotate r)
    rotate _ = Leaf -- todo
    height :: Tree a -> Integer
    height Leaf = 0
    height (Node h _ _ _) = h + 1
    balanced :: Tree a -> Bool
    balanced Leaf = True
    balanced (Node _ l _ r) = balanced l && balanced r && abs (height l - height r) <= 1

-- leftRotate :: Tree a -> Tree a
-- leftRotate (Node h t1 v t2) =
-- rightRotate :: Tree a -> Tree a

xor :: [Bool] -> Bool
xor = foldr Data.Bits.xor False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a -> (f a :)) []

-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr () base xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram x = tail $ takeWhile (< 2 * x + 2) primes
  where
    primes :: [Integer]
    primes = 2 : findPrimes [3, 5 ..]
    findPrimes :: [Integer] -> [Integer]
    findPrimes l@(x : xs) = x : findPrimes (xs \\ map (x *) l)