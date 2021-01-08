solve :: [Int] -> String
solve (a:b:c:[]) 
    | ca < cb   = "Cat A"
    | cb < ca   = "Cat B"
    | otherwise = "Mouse C"
    where
        ca = abs $ c - a
        cb = abs $ c - b

main :: IO ()
main = interact $ unlines . map solve . map (map read . words) . tail . lines
