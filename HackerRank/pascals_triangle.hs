-- Pascals Triangle
-- HackerRank

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x:xs) = x : dropLast xs

map2 :: (a -> a -> b) -> [a] -> [a] -> [b]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys

-- Naive Version
-- triag :: Int -> Int -> [Int] -> [[Int]]
-- triag i n xs
--   | i == n = [xs]
--   | otherwise = xs : triag (i+1) n ( (1 : map2 (+) l1 l2) ++ [1])
--   where
--     l1 = tail xs
--     l2 = dropLast xs

triaglist :: [[Int]]
triaglist = [1] : [1, 1] : nexttriag [1, 1]
  where
    nexttriag xs =
      l3 : nexttriag l3
      where
        l1 = tail xs
        l2 = dropLast xs
        l3 = (1 : map2 (+) l1 l2) ++ [1]

main :: IO ()
main =
  interact
  $ unlines
  . map (unwords . map show)
  . (`take` triaglist)
  . (read :: String -> Int)
