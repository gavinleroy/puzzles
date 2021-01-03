fmt :: (Float, Float, Float) -> String
fmt (x, y, z) = unlines [show x, show y, show z]

solve :: [Int] -> (Float, Float, Float)
solve ns = (pos / len, neg / len, zer / len)
    where
        pos = fromIntegral $ length $ filter (>0) ns
        neg = fromIntegral $ length $ filter (<0) ns
        zer = fromIntegral $ length $ filter (==0) ns
        len = fromIntegral $ length ns 

main :: IO ()
main = interact $ fmt 
                . solve 
                . map read
                . words 
                . head
                . tail 
                . lines
