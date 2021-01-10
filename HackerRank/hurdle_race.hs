solve :: [Int] -> Int
solve (_:k:xs) 
    | mm > 0      = mm
    | otherwise   = 0
    where mm = maximum xs - k
    
main :: IO ()
main = interact $ show . solve . map read . words

