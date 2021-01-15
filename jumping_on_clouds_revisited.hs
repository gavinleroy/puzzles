import Control.Monad

solve :: Int -> Int -> [Int] -> Int
solve e k cs@(0:cs') = solve (e - 1) k $ drop k cs
solve e k cs@(1:cs') = solve (e - 3) k $ drop k cs
solve e k cs@(2:cs') = e


main :: IO ()
main = do
    [[_, k], cs] <- replicateM 2 (map read . words <$> getLine)
    let e = if head cs == 0 then 99 else 97
    putStrLn $ show $ solve e k $ drop k $ cycle (2 : tail cs)
    
