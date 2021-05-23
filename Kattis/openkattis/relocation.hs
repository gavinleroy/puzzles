-- Kattis - Relocation

import Control.Monad
import Data.Array.IO

lineToInts :: String -> [Int]
lineToInts = fmap read . words

-- ! using mutable IOArray for speed
solve :: IOArray Int Int -> [Int] -> IO ()
solve ar [1, c, x] =
  writeArray ar c x
solve ar [2, a, b] = do
  a' <- readArray ar a
  b' <- readArray ar b
  print $ abs (a' - b')
solve _ _ = error "impossible"
             
main :: IO ()
main = do
  [n, q] <- lineToInts <$> getLine
  origVs <- lineToInts <$> getLine
  arr    <- newListArray (1,n) origVs
  forM_ [1..q] $ \ _ -> do
    query <- lineToInts <$> getLine
    solve arr query
