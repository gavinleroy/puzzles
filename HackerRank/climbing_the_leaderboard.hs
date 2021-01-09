import Control.Monad
import Data.List hiding (find)

readIntList :: IO [Int]
readIntList = do
    ln <- getLine
    return $ map read $ words ln
    
solve' :: [(Int, Int)] -> [Int] -> [Int]
solve' _ [] = []
solve' [] (v:vs) = 1 : solve' [] vs
solve' ts@((i, p):ts') vs@(v:vs')
    | v > p  = solve' ts' vs
    | v == p = i : solve' ts vs'
    | v < p  = i + 1 : solve' ts vs'

solve :: [Int] -> [Int] -> [Int]
solve = solve' . reverse . zip [1..] . fmap head . group

main :: IO ()
main = do
    [_, ranks, _, players] <- replicateM 4 readIntList
    let rArrowP = solve ranks
    putStr $ unlines $ map show $ solve ranks players
