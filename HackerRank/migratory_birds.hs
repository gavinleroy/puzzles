 import Data.List
 
 cmp :: [a] -> [a] -> Ordering
 cmp xs ys
    | lx < ly   = GT
    | lx == ly  = EQ
    | otherwise = LT
    where lx = length xs
          ly = length ys
 
 solve :: [Int] -> Int
 solve = head . head . sortBy cmp . group . sort
 
 main :: IO ()
 main = interact $ show . solve . map read . tail . words

