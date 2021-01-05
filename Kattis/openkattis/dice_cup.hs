solve :: [Int] -> [Int]
solve vs = [min+1..max+1]
  where
    min = minimum vs
    max = maximum vs

main :: IO ()
main = interact $ unlines . map show . solve . map read . words
