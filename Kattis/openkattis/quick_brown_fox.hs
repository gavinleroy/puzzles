-- Kattis - quick brown fox

import Data.Char (isLetter, toLower)
import Data.List (sort, group, (\\))
  
test1 = "The quick brown fox jumps over the lazy dog."
test2 = "ZYXW, vu TSR Ponm lkj ihgfd CBA."
test3 = ".,?!'\" 92384 abcde FGHIJ"

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"
  
rmdups :: (Ord a) => [a] -> [a]
rmdups =
  map head . group . sort

sanitize :: String -> String
sanitize =
  sort . rmdups . fmap toLower . filter isLetter

solve :: String -> String
solve s =
  let res = alphabet \\ sanitize s in
  if res == "" then
    "pangram"
  else "missing " ++ res

main :: IO ()
main = interact $ unlines . fmap solve . tail . lines
  
