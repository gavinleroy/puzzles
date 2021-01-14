import Data.Vector hiding (map, replicateM, forM_)
import Control.Monad

solve :: Int -> Int -> [Int] -> [Int] -> [Int]
solve n k vs qs = map (((!) vvs) . i) qs
    where 
        vvs = fromList vs
        i = (\x -> ((n - (k `mod` n)) + x) `mod` n)

main :: IO ()
main = do
    [[n, k, q], ns] <- replicateM 2 (map read . words <$> getLine)
    qs              <- replicateM q (read <$> getLine)
    forM_ (solve n k ns qs) (putStrLn . show)
