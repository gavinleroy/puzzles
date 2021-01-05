solve :: [Int] -> String
solve (n:k:rest)
    | topay /= charged = show $ charged - topay
    | otherwise        = "Bon Appetit"
    where
        topay = (sum vals - vals !! k) `div` 2
        charged = head chrg
        (vals, chrg) = splitAt n rest

main :: IO ()
main = interact $ solve . map read . words
