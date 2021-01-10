import Data.List
import Control.Monad

readInt :: IO Int
readInt = do
    v <- getLine
    return $ read v
    
readIntTuple :: IO (Int, Int)
readIntTuple = do
    ln <- getLine
    return $ (\l -> (l !! 0, l !! 1)) $ map read $ words ln
    
cmp :: Ord a => (a -> a -> b) -> (a, a) -> (a, a) -> b
cmp = \e x y -> e (fst x) (fst y)

equal :: (Eq a) => [a] -> Bool
equal [] = True
equal xs = and $ map (== head xs) (tail xs)

answer :: Bool -> String
answer True  = "YES"
answer False = "NO"

solve :: [(Int, Int)] -> String
solve = answer
    . and
    . map equal 
    . map (map snd) 
    . groupBy (cmp (==)) 
    . sortBy (cmp compare)

main :: IO ()
main = do
    t <- readInt
    forM_ [1..t] $ \_ -> do
       q <- readInt
       vs <- replicateM q readIntTuple
       putStrLn $ solve vs


