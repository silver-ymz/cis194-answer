module SExpr where

import           AParser
import           Control.Applicative
import           Data.Char

-- exercise 1

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

oneOrMore :: Parser a -> Parser [a]
oneOrMore = some

-- exercise 2

spaces :: Parser String
spaces = many $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

-- exercise 3

type Ident = String

data Atom = N Integer | I Ident deriving Show

data SExpr = A Atom | Comb [SExpr] deriving Show

parseSAtom :: Parser Atom
parseSAtom = N <$> posInt <|> I <$> ident

parseSExpr :: Parser SExpr
parseSExpr = spaces *> ((A <$> parseSAtom) <|> char '(' *> (Comb <$> some parseSExpr) <* char ')')

main = print $ runParser parseSExpr " (bar (foo) 3 5 874)"
