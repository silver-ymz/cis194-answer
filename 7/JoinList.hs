{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import           Buffer
import           Editor
import           Scrabble
import           Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Show, Eq)

-- exercise 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l +++ r = Append (tag l <> tag r) l r

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single v _)   = v
tag (Append v _ _) = v

-- exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ index (Single _ v)
  | index == 0 = Just v
  | otherwise  = Nothing
indexJ index (Append n l r)
  | index <  0     = Nothing
  | index <  sizeL = indexJ index l
  | index <= sizeA = indexJ (index - sizeL) r
  | otherwise = Nothing
  where sizeA = getSize . size $ n
        sizeL = getSize . size . tag $ l


(!!?) :: [a] -> Int -> Maybe a
[]       !!? _         = Nothing
_        !!? i | i < 0 = Nothing
(x : xs) !!? 0         = Just x
(x : xs) !!? i         = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n s@(Single _ _) | n <= 0 = s
dropJ n s@(Append m l r)
  | n <= 0 = s
  | n < sizeL = dropJ n l +++ r
  | n <= sizeA = dropJ (n - sizeL) r
  where sizeA = getSize . size $ m
        sizeL = getSize . size . tag $ l
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n s@(Single _ _) | n > 0 = s
takeJ n s@(Append m l r)
  | n <= 0 = Empty
  | n <= sizeL = takeJ n l
  | n <= sizeA = l +++ takeJ (n - sizeL) r
  | otherwise = s
  where sizeA = getSize . size $ m
        sizeL = getSize . size . tag $ l
takeJ _ _ = Empty

-- exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldl (\l s -> l +++ Single (scoreString s, 1) s) Empty .lines
  line = indexJ
  replaceLine n str l = takeJ n l +++ fromString str +++ dropJ (n+1) l
  numLines = getSize . snd . tag
  value = getScore . fst . tag


main = runEditor editor (fromString "test" :: (JoinList (Score, Size) String))
