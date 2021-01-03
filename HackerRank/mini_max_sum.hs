import Data.List

-- You can defined a really short sort algo :)
-- quicksort :: Ord a => [a] -> [a]
-- quicksort []     = []
-- quicksort [x]    = [x]
-- quicksort (x:xs) = lowers ++ [x] ++ uppers
--     where
--         lowers = quicksort $ filter (<=x) xs
--         uppers = quicksort $ filter (>x) xs

solve :: [Int] -> String
solve ns = show min ++ " " ++ show max
    where
        min  = sum $ take 4 srtd
        max  = sum $ drop 1 srtd
        srtd = sort ns

main :: IO ()
main = interact $ solve . map read . words
