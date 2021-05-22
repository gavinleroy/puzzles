-- HackerRank - perimeter of a polygon

test1 :: [Point]
test1 =
      [ (1043, 770)
      , (551, 990)
      , (681, 463) ]

-- type Point = (Int, Int)
type Point = (Float, Float)

dist :: Point -> Point -> Float
dist (x, y) (x', y') =
  sqrt $ (x' - x)**2 + (y' - y)**2

calcPerimeter :: [Point] -> Float
calcPerimeter (p1 : p2 : ps) =
  dist p1 p2 + calcPerimeter (p2 : ps)
calcPerimeter _ = 0.0

solve :: [Point] -> Float
solve z@(x : _) =
  calcPerimeter $ z ++ [x]
solve _ = error "impossible"

readPoint :: String -> Point
readPoint =
  (\ [s1, s2] -> (s1, s2)) . fmap read . words

readPointList :: String -> [Point]
readPointList =
  fmap readPoint . tail . lines

main :: IO ()
main =
  interact $ show . solve . readPointList
