num :: Char -> Int
num c = if c == 'U' then 1 else -1

rmdups :: [Int] -> [Int]
rmdups []       = []
rmdups (n:ns) 
    | n < 0     = n : rmdups (dropWhile (<0) ns)
    | otherwise = rmdups ns

makelist :: Int -> String -> [Int]
makelist _ []     = []
makelist n (c:cs) = next : makelist next cs
    where next = n + num c

main :: IO ()
main = interact $ show . length . rmdups . makelist 0 . head . tail . words
