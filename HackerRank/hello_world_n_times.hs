{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

printHW :: Int -> IO()
printHW 1 = putStrLn "Hello World"
printHW a = do
    printHW (a - 1)
    putStrLn "Hello World"


main :: IO()
main = do
    n <- readLn :: IO Int
    printHW n

