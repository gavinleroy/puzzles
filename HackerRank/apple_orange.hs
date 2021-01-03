solve :: [Int] -> [Int]
solve (s:t:a:b:m:_:ns) = [ains, bins]
    where 
        ains = length $ filter p as
        bins = length $ filter p bs
        p = \x -> s <= x && x <= t
        as = map (+a) $ take m ns
        bs = map (+b) $ drop m ns

main :: IO ()
main = interact $ unlines . map show . solve . map read . words 
