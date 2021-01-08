solve :: [Int] -> Int
solve (n:pg:[]) = minimum [front, back]
    where
        front = pg `div` 2
        back  = (n `div` 2) - (pg `div` 2)
        

main :: IO ()
main = interact $ show . solve . map read . words

