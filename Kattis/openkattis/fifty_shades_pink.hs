-- Kattis, Fifty Shades of Pink

import Data.Char (toLower)
import Data.List (isInfixOf)

test1 =
  [ "pink"
  , "tequilaSunrose"
  , "mExicanPInK"
  , "Coquelicot"
  , "turqrose"
  , "roSee"
  , "JETblack"
  , "pink"
  , "babypink"
  , "pInKpinkPinK"
  , "PInkrose"
  , "lazerlemon" ]

test2 =
  [ "roose"
  , "rosse"
  , "pinnk"
  , "screw"
  , "cerise"
  , "magenta" ]

haspink :: String -> Bool
haspink = isInfixOf "pink"

hasrose :: String -> Bool
hasrose = isInfixOf "rose"

isvalid :: String -> Bool
isvalid s = haspink s || hasrose s

lowerword :: String -> String
lowerword = fmap toLower

solve :: [String] -> Either Int String
solve cs
  | nums == 0 = Right "I must watch Star Wars with my daughter"
  | otherwise = Left nums
  where nums = length $ filter isvalid $ fmap lowerword cs
  
main :: IO ()
main = interact $ either show id . solve . tail . lines
