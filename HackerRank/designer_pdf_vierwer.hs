getIntList :: IO [Int]
getIntList = do
    ln <- getLine
    return $ map read $ words ln
    
solve :: [Int] -> [Char] -> Int
solve heights word = length word * h
    where h = maximum [height |  (height, letter) <- zip heights ['a'..'z'],
                                 c <- word,  c == letter]

main :: IO ()
main = do
    heights <- getIntList
    word <- getLine
    putStrLn $ show $ solve heights word
