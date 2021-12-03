-- Gavin Gray AOC

module Main where

import Data.Char (digitToInt)
import Data.List (transpose)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

binToInt :: [Int] -> Int
binToInt = sum . zipWith (*) (iterate (*2) 1)

majority :: [Char] -> Char
majority xs = if zc > oc then
                '0'
              else '1'
  where zc = count '0' xs
        oc = count '1' xs

minority :: [Char] -> Char
minority xs = case majority xs of
  '0' -> '1'
  '1' -> '0'
  _ -> error "Impossible"

getBased :: ([Char] -> Char) -> [[Char]] -> Int
getBased f cs = binToInt $ map digitToInt $ loop (transpose cs) []
  where
    loop [] acc = acc
    loop as@(l : _) acc = case rst of
      (_ : _: _) -> loop (tail $ transpose rst) (v : acc)
      [only] -> reverse only ++ acc
      _ -> error "Impossible"
      where v   = f l
            rst = filter (\(fv : _) -> fv == v) $ transpose as

getRates :: [[Char]] -> (Int, Int)
getRates cs = (getBased majority cs, getBased minority cs)

solve :: [[Char]] -> Int
solve = uncurry (*) . getRates

main :: IO ()
main = interact $ show . solve . lines
