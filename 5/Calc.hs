{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Data.Map ((!?))
import qualified Data.Map as M
import ExprT
import Parser
import qualified StackVM

-- exercise 1

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- exercise 2

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

-- exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- exercise 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- exercise 5

instance Expr StackVM.Program where
  lit n = [StackVM.PushI n]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

parse :: Expr a => String -> Maybe a
parse = parseExp lit add mul

compile :: String -> Maybe StackVM.Program
compile = parse

-- exercise 6

class HasVars a where
  var :: String -> a

newtype VarExprT = VarExprT ExprT

instance Expr VarExprT where
  lit = VarExprT . Lit
  add (VarExprT a) (VarExprT b) = VarExprT $ Add a b
  mul (VarExprT a) (VarExprT b) = VarExprT $ Mul a b

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = flip (!?)

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const $ const $ Just 3
  add a b m = (+) <$> a m <*> b m
  mul a b m = (*) <$> a m <*> b m

withVars ::
  [(String, Integer)] ->
  (M.Map String Integer -> Maybe Integer) ->
  Maybe Integer
withVars vs exp = exp $ M.fromList vs

main = print (withVars [("x", 6)] $ add (lit 3) (var "x")) >> print (withVars [("x", 6)] $ add (lit 3) (var "y")) >> print (withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x")))
