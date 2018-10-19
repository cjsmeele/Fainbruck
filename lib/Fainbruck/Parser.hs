module Fainbruck.Parser (Fainbruck.ParserToolkit.parse,
                         expr) where

import Fainbruck.ParserToolkit
import Control.Monad
import Control.Applicative

import Fainbruck.Expr

plus     =    Add   1  <$ char '+'
minus    =    Add (-1) <$ char '-'
ptrplus  = PtrAdd   1  <$ char '>'
ptrminus = PtrAdd (-1) <$ char '<'
input    =  Input      <$ char ','
output   = Output      <$ char '.'

block = do char '['
           xs <- many expr
           char ']'
           return $ Block xs

expr :: Parser Expr
expr = do many bad
          x <- expr'
          many bad
          return x

    where expr' =  ptrplus
               <|> ptrminus
               <|> plus
               <|> minus
               <|> input
               <|> output
               <|> block
          bad = noneOf "><+-.,[]"

