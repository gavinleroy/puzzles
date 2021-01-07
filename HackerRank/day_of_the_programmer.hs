import Data.List
import Text.Printf

leapday :: Int -> String
leapday = printf "12.09.%d"

normalday :: Int -> String
normalday = printf "13.09.%d"

julian :: Int -> String
julian year
    | year `mod` 4 == 0 = leapday year
    | otherwise         = normalday year

gregorian :: Int -> String
gregorian year
    | year `mod` 400 == 0 || 
    (year `mod` 4 == 0 && year `mod` 100 /= 0) = leapday year
    | otherwise                                = normalday year
    
solve :: Int -> String
solve year
    | year <= 1917 = julian year
    | year == 1918 = "26.09.1918"
    | otherwise    = gregorian year
    
main :: IO ()
main = interact $ solve . read

