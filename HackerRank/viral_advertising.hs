shared :: [Int]
shared = 5 : 6 : map (\x -> (x `div` 2) * 3) (tail shared)

solve :: Int -> Int
solve n = sum $ map (`div` 2) $ take n shared

main :: IO ()
main = interact $ show . solve . read
