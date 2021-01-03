import Prelude hiding (round)

round :: Int -> Int
round n 
  | n < 38        = n
  | d < 3 = n + d
  | otherwise     = n
  where
    d = 5 - (n `mod` 5)

main :: IO ()
main = interact $ unlines 
                . map show 
                . map round 
                . map read 
                . tail 
                . lines
