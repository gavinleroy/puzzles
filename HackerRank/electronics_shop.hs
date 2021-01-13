import Control.Monad

solve' :: [Int] -> Int
solve' [] = -1
solve' ns = maximum ns

solve :: Int -> [Int] -> [Int] -> Int
solve b keys mice = solve' $ filter (<=b) $ (+) <$> keys <*> mice
        
ril :: IO [Int]
ril = map read . words <$> getLine

main :: IO ()
main = do
    [[b, _, _], keys, mice] <- replicateM 3 ril
    putStrLn $ show $ solve b keys mice

