import Data.List

type MS = [[Int]]

magicsquare :: MS
magicsquare = [[4, 3, 8], [9, 5, 1], [2, 7, 6]]

rot90 :: MS -> MS
rot90 = map reverse . transpose

allsquares = (take 4 $ iterate rot90 magicsquare) ++ 
             (take 4 $ iterate rot90 (transpose magicsquare))

cost :: MS -> MS -> Int
cost s1 s2 = sum $ map abs $ zipWith (-) (concat s1) (concat s2)

solve :: MS -> Int
solve sq = minimum $ map (cost sq) allsquares

main :: IO ()
main = interact $ show . solve . map (map read . words) . lines
