-- Kattis, Musical Scales

import Data.List (inits, tails, isInfixOf)

type Note = String

tests = [ words "C D F D C D F F F C"
        , words "A B A F# G# C" ]

pattern :: [Int]
pattern = [ 0, 1, 1, 0, 1, 1, 1, 0 ]

notes :: [Note]
notes = [ "A" , "A#" , "B"
        , "C" , "C#" , "D"
        , "D#" , "E" , "F"
        , "F#" , "G" , "G#" ]
  
getscale :: [Note] -> [Int] -> [Note]
getscale _ [] = []
getscale ns (i:is) = ns !! i : getscale (drop (i+1) ns) is

rotations :: [a] -> [[a]]
rotations xs = init $ zipWith (++) (tails xs) (inits xs)

genallscales :: [Note] ->  [Int] -> [[Note]]
genallscales ns is
  = map (\ns' -> getscale (cycle ns') is)
  $ rotations ns

-- generated using: genallscales notes pattern
scales :: [[Note]]
scales =
  [["A","B","C#","D","E","F#","G#","A"]
  ,["A#","C","D","D#","F","G","A","A#"]
  ,["B","C#","D#","E","F#","G#","A#","B"]
  ,["C","D","E","F","G","A","B","C"]
  ,["C#","D#","F","F#","G#","A#","C","C#"]
  ,["D","E","F#","G","A","B","C#","D"]
  ,["D#","F","G","G#","A#","C","D","D#"]
  ,["E","F#","G#","A","B","C#","D#","E"]
  ,["F","G","A","A#","C","D","E","F"]
  ,["F#","G#","A#","B","C#","D#","F","F#"]
  ,["G","A","B","C","D","E","F#","G"]
  ,["G#","A#","C","C#","D#","F","G","G#"]]

allin :: [Note] -> [Note] -> Bool
allin qs ns = all (`elem` ns) qs

solve' :: [Note] -> [Note]
solve' qs = map fst
  $ filter snd
  $ zip notes
  $ map (qs `allin`) scales

solve :: [Note] -> String
solve ns = case solve' ns of
  [] -> "none"
  xs -> unwords xs
  
main :: IO ()
main = interact $ solve . tail . words
