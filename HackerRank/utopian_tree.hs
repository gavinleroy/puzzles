-- Naïve Solution, better to use Data.Vector for indexing --
heights :: [Integer]
heights = 1 : 2 : map (\x -> if even x then x + 1 else x * 2) (tail heights)

main :: IO ()
main = interact $  unlines . map (show . ((!!) heights) . read) . tail . words
