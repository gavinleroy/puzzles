solve :: [Int] -> Int
solve (i:j:k:_) = length [() | day <- [i..j], 
    (abs (day - (read $ reverse $ show day))) `mod` k == 0]

main :: IO ()
main = interact $ show . solve . map read . words

