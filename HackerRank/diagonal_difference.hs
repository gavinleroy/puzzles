import Data.List.Split

diagsums :: [[Int]] -> (Int, Int)
diagsums m = (diagsum m, diagsum $ map reverse m)

diagsum  :: [[Int]] -> Int
diagsum m = sum [m !! n !! n | n <- [0..t]]
    where t = length m - 1

readmatr :: String -> [[Int]]
readmatr ss = chunksOf l $ tail s
    where l  = head s
          s =  map read $ words ss

absdiff :: (Int, Int) -> Int
absdiff (a, b) = abs $ b - a

main :: IO ()
main = interact $ show . absdiff . diagsums . readmatr
