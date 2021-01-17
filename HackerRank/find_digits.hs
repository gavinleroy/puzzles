digits :: Int -> [Int]
digits 0 = []
digits n = n `mod` 10 : (digits (n `div` 10))

solve :: Int -> Int
solve n = length [() | k <- digis, k /= 0 && n `mod` k == 0]
    where digis = digits n

main :: IO ()
main = interact $ unlines . map (show . solve . read) . tail . words
