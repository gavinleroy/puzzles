-- Removing concatenation --
getsets' :: [Int] -> [Int] -> [Int]
getsets' as [] = as
getsets' as (n:ns) =  getsets' (lt:gt:as) ns
    where gt = (+) 1 $ length $ filter (\x -> x == n || x == n+1) ns
          lt = (+) 1 $ length $ filter (\x -> x == n || x == n-1) ns
          
getsets :: [Int] -> [Int]
getsets = getsets' []
          
-- Original solution with concatenation --
-- getsets :: [Int] -> [Int]
-- getsets [] = []
-- getsets (n:ns) =  [lt, gt] ++ getsets ns
--     where gt = 1 + (length $ filter (\x -> x == n || x == n+1) ns)
--           lt = 1 + (length $ filter (\x -> x == n || x == n-1) ns)
          
main :: IO ()
main = interact $ show . maximum . getsets . map read . tail . words
