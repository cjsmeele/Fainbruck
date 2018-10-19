module Main where

import Fainbruck.Interpreter

import System.Environment
import System.IO

main :: IO ()
main = do args     <- getArgs
          progName <- getProgName
          hSetBuffering stdout NoBuffering
          hSetBuffering stdin  NoBuffering
          case args of
            [s] -> runFile s
            _   -> putStrLn $ "usage: " ++ progName ++ " FILE"
