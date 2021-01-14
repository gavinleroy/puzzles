import Data.Function
import Data.List

solve :: [Int] -> [Int]
solve xs = [snd $ (!!) vs (i-1) | (_, i) <- vs]
    where vs = sortBy (compare `on` fst) $ zip xs [1..]

main :: IO ()
main = interact $ unlines . map show . solve . map read . tail . words
