buildstep :: Int -> Int -> String
buildstep tot hash = (replicate (tot - hash) ' ') ++ (replicate hash '#')

buildcase :: Int -> [String]
buildcase n = [buildstep n x | x <- [1..n]]

main :: IO ()
main = interact $ unlines . buildcase . read
