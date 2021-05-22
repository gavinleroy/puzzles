-- Kattis - Zamka

import Data.Char (digitToInt)

sumds :: Int -> Int
sumds =
  sum . fmap digitToInt . show

candidates :: Int -> Int -> Int -> [Int]
candidates l d x =
  [c | c <- [l..d + 1], sumds c == x]

solve :: [Int] -> [Int]
solve [l,d,x] =
  let cs = candidates l d x in
    [minimum cs, maximum cs]
solve _ = error "impossible"

main :: IO ()
main = interact
  $ unlines
  . fmap show
  . solve
  . fmap (read :: String -> Int)
  . lines
