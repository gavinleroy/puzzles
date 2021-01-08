solve' :: [Int] -> Int
solve' [] = -1
solve' ns = maximum ns

solve :: [Int] -> Int
solve (b:n:_:ns) = solve' [k + m | k <- keys, m <- mice, k+m <= b]
    where
        (keys, mice) = splitAt n ns

main :: IO ()
main = interact $ show . solve . map read . words
