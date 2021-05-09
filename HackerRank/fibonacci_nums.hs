-- Fibonacci Numbers
-- HackerRank

fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)

main :: IO ()
main = interact $ show . (fib !!) . (\x -> x - 1) . (read :: String -> Int)
