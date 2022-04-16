-- exercise 1
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

fibs1 :: [Integer]
fibs1 = fib <$> [0 ..]

-- exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f fir = Cons fir $ streamFromSeed f $ f fir

-- exercise 5

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x s) y = Cons x $ streamInterleave y s

ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) (streamMap (+ 1) ruler)

-- exercise 6

streamZip :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZip f (Cons xa sa) (Cons xb sb) = Cons (f xa xb) $ streamZip f sa sb

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger x = Cons x $ streamRepeat 0
  negate = streamMap negate
  (+) = streamZip (+)
  (*) (Cons xa sa) b@(Cons xb sb) = Cons (xa * xb) $ streamMap (xa *) sb + sa * b

instance Fractional (Stream Integer) where
  (/) (Cons xa sa) (Cons xb sb) = let q = Cons (xa `div` xb) $ streamMap (`div` xb) (sa - sb * q) in q

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

-- exercise 7

data Matrix = Matrix Integer Integer Integer Integer deriving (Show, Eq)

instance Num Matrix where
  (*) (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) = Matrix (a1 * b1 + a2 * b3) (a1 * b2 + a2 * b4) (a3 * b1 + a4 * b3) (a3 * b2 + a4 * b4)

fib4 :: Integer -> Integer
fib4 n = ans
  where
    Matrix _ ans _ _ = Matrix 1 1 1 0 ^ n

main = print fibs3