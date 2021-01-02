solve :: [Int] -> Int
solve ns = ans
    where
        ans = length $ filter (== max) ns
        max = maximum ns
        
main :: IO ()
main = interact $ show . solve . map read . words . head . tail . lines
