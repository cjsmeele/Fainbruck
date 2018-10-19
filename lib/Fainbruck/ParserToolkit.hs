module Fainbruck.ParserToolkit where

import Control.Monad
import Control.Applicative

newtype Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
    fmap g p = p >>= return . g

instance Applicative Parser where
    pure x    = P (\s -> Just (x,s))
    pf <*> px = pf >>= (<$> px)

instance Monad Parser where
    px >>= f = P (\s -> case parse px s of
                          Just (x,rest) -> parse (f x) rest
                          Nothing       -> Nothing)

instance Alternative Parser where
  empty     = P (const Nothing)
  px <|> py = P (\s -> parse px s <|> parse py s)

instance MonadPlus Parser

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

item :: Parser Char
item = P g
    where g []     = Nothing
          g (x:xs) = pure (x,xs)

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= g
    where g x | p x       = pure x
              | otherwise = empty

char :: Char -> Parser Char
char c = sat (== c)

noneOf :: Foldable t => t Char -> Parser Char
noneOf xs = sat (not . (`elem` xs))
