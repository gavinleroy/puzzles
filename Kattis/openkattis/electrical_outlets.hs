-- Kattis - Electrical Outlets

import Control.Monad

solve :: [Integer] -> Integer
solve =
  foldl (\ acc v ->
           acc - 1 + v) 1 

main :: IO ()
main = do
  n <- (read :: String -> Int) <$> getLine
  forM_ [1..n] $ \ _ -> do
    xs <- fmap (read :: String -> Integer) . tail . words <$> getLine
    print $ solve xs
        
