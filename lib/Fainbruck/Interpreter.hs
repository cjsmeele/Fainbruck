module Fainbruck.Interpreter (runExprs, runStr, runFile) where

import Control.Monad
import Control.Applicative
import Fainbruck.Expr
import Fainbruck.Parser
import Fainbruck.Eval

runExprs :: [Expr] -> IO ()
runExprs xs = do let simp = simplify xs
                 st <- newState
                 evals st simp
                 --putStrLn $ "expr: " ++ show xs
                 --putStrLn $ "simp: " ++ show simp

runStr :: String -> IO ()
runStr s = case parse (many expr) s of
             Just (xs,[]) -> runExprs xs
             _            -> putStrLn "bah!"

runFile :: String -> IO ()
runFile = readFile >=> runStr
