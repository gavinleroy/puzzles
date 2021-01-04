-- x1 <= x2
-- x1 + k v1 = x2 + k v2
-- x2 - x1 = k (v2 - v1)
solve :: [Int] -> String
solve [x1, v1, x2, v2]
    | v2 >= v1                       = "NO"
    | (x2 - x1) `mod` (v2 - v1) == 0 = "YES"
    | otherwise                      = "NO"

main :: IO ()
main = interact $ solve . map read . words
