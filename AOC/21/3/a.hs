-- Gavin Gray AOC

module Main where

import Data.Tuple (swap)
import Data.List  (transpose)

mapTuple :: ([a] -> a) -> ([a], [a]) -> (a, a)
mapTuple f (x, y) = (f x, f y)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

binToInt :: [Int] -> Int
binToInt = sum . zipWith (*) (iterate (*2) 1)

push :: ([a], [a]) -> (a, a) -> ([a], [a])
push (xs, ys) (x, y) = (x : xs, y : ys)

flipGuard :: Bool -> (a, a) -> (a, a)
flipGuard True  = id
flipGuard False = swap

majority :: [Char] -> Bool
majority xs = count '0' xs >= count '1' xs

getRates :: [[Char]] -> (Int, Int)
getRates cs = mapTuple binToInt
  $ foldl (\acc l -> push acc
          $ flip flipGuard p
          $ majority l ) ([], [])
  $ transpose cs
  where p = (0, 1)

solve :: [[Char]] -> Int
solve = uncurry (*) . getRates

main :: IO ()
main = interact $ show . solve . lines
