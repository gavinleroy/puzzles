stringsolve :: (Int, Int) -> String
stringsolve (x, y) = show x ++ " " ++ show y

solve :: ([Int], [Int]) -> (Int, Int)
solve ([], _)      = (0, 0)
solve (_, [])      = (0, 0)
solve ((x:xs), (y:ys)) 
    | x > y     = (1 + lhs, rhs)
    | x < y     = (lhs, rhs + 1)
    | otherwise = (lhs, rhs)
    where (lhs, rhs) = solve (xs, ys)

readIntLine :: String -> ([Int], [Int])
readIntLine s = (take 3 intlist, drop 3 intlist)
    where intlist = map read $ words s


main :: IO ()
main = interact $ stringsolve . solve . readIntLine

