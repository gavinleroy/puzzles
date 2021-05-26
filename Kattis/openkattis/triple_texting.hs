-- Kattis, Triple Texting

tests =
  [ "hellohrllohello"
  , "hejhejhej" ]

chunksof :: Int -> [a] -> [[a]]
chunksof _ [] = []
chunksof n xs =
  take n xs : chunksof n (drop n xs)

merge :: Eq a => (a, a, a) -> a
merge (c1, c2, c3)
  | c1 == c2 = c1
  | c2 == c3 = c2
  | c3 == c1 = c3
  | otherwise = error "impossible"

solve :: String -> String
solve cs = map merge $ zip3 w1 w2 w3
  where
    (w1:w2:w3:_) = chunksof ws cs
    ws = length cs `div` 3

main :: IO ()
main = interact solve
