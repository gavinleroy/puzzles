import Control.Monad

tostr :: Bool -> String
tostr b = if b then "NO" else "YES"

solve :: Int -> [Int] -> Bool
solve k = (<=) k . length . filter (<=0)

readIL :: IO [Int]
readIL = getLine >>= return . map read . words

main :: IO ()
main = do
    t <- readLn :: IO Int
    forM_ [1..t] (\_ -> do
        [_, k] <- readIL
        times <- readIL
        putStrLn $ tostr $ solve k times)
