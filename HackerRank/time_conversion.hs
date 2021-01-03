import Text.Printf

fmt :: Int -> Int -> Int -> String
fmt h m s = printf "%02d" h ++ ":" ++ printf "%02d" m ++ ":" ++ printf "%02d" s

solve :: Int -> Int -> Int -> Bool -> String
solve h m s pm 
    | pm && h /= 12      = fmt (h+12) m s
    | not pm && h == 12  = fmt 0 m s
    | otherwise          = fmt h m s

parse :: String -> String
parse cs = solve h m s (pm == "PM")
    where
        (h, m, s) = (read (brk !! 0), read (brk !! 1), read scd)
        pm = drop 2 lst
        scd = take 2 lst
        lst = brk !! 2
        brk = words rep 
        rep = replace cs
        replace = map (\c -> if c==':' then ' '; else c)
        
main :: IO ()
main = interact $ parse


