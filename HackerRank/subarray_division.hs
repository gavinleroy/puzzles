solve_ :: Int -> Int -> [Int] -> Int
solve_ len su ns
  | length ns < len = 0
  | sum tosum == su = 1 + (solve_ len su $ tail ns)
  | otherwise       = solve_ len su $ tail ns
  where tosum = take len ns

solve :: [Int] -> Int
solve ns = solve_ (inp !! 1) (inp !! 0) (take len $ tail ns) 
  where len = head ns
        inp = drop len $ tail ns

main :: IO ()
main = interact $ show . solve . map read . words
