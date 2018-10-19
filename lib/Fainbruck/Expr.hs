module Fainbruck.Expr where

data Expr = PtrAdd Int
          | Add Int
          | Input
          | Output
          | Block [Expr]
          | Zero
          deriving Show

simplify :: [Expr] -> [Expr]
simplify [] = []
-- Squash adds.
simplify (PtrAdd x : PtrAdd y : xs) = simplify (PtrAdd (x+y) : simplify xs)
simplify (   Add x :    Add y : xs) = simplify (   Add (x+y) : simplify xs)
-- Remove pointless actions.
simplify (PtrAdd 0 :            xs) = simplify xs
simplify (   Add 0 :            xs) = simplify xs
simplify (   Add _ :    Input : xs) = simplify (Input : simplify xs)
-- Special-case add-until-zero loops.
simplify (Block [Add _] :       xs) = Zero : simplify xs

simplify (Block x : xs) = Block (simplify x) : simplify xs
simplify (x : xs) = x : simplify xs
