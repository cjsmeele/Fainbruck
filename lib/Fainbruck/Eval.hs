module Fainbruck.Eval where

import Fainbruck.Expr
import Control.Monad
import Data.Word
import Data.Char (ord,chr)
import Data.Vector.Unboxed.Mutable as V
import Data.IORef

type Cell = Word8

data State = State { tape :: V.IOVector Cell
                   , ptr  :: IORef      Int
                   }

-- Read pointer.
rptr :: State -> IO Int
rptr st = readIORef (ptr st)

-- Write pointer.
wptr :: State -> Int -> IO ()
wptr st = writeIORef (ptr st)

-- Read current cell.
rcur :: State -> IO Cell
rcur st = do p <- readIORef (ptr st)
             V.read (tape st) p

-- Write current cell.
wcur :: State -> Word8 -> IO ()
wcur st x = do p <- readIORef (ptr st)
               V.write (tape st) p x

-- This is much faster than an unconditional (x `mod` mx).
wrap :: Int -> Int -> Int
wrap mx x | x < 0 || x >= mx = x `mod` mx
          | otherwise        = x

-- Evaluate an expression.
eval :: State -> Expr -> IO ()
eval st (PtrAdd n) = do x <- rptr st
                        wptr st $ wrap 30000 (x + n)
eval st (Add n)    = do x <- rcur st
                        wcur st $ x + fromIntegral n
eval st Zero       = wcur st 0
eval st Input      = do x <- getChar
                        wcur st $ fromIntegral $ ord x
eval st Output     = do x <- rcur st
                        putChar $ chr $ fromIntegral x
eval st (Block xs) = do x <- rcur st
                        when (x /= 0) $
                          do evals st xs
                             eval  st (Block xs)

-- Evaluate multiple expression.
evals :: State -> [Expr] -> IO ()
evals st = mapM_ (eval st)

-- Create an empty state.
newState :: IO State
newState = do t <- V.new 30000
              V.set t 0
              p <- newIORef 0
              return State { tape = t,
                             ptr  = p }
