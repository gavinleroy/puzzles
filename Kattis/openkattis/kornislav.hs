-- Kattis, Kornisalv

import Data.List (sort)

solve :: [Int] -> Int
solve xs =
  a * c
  where [a, _, c, _] = sort xs

main :: IO ()
main = interact $ show . solve . fmap (read :: String -> Int) . words 
