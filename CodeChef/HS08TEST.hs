import Text.Printf
import GHC.Float

withdraw :: Int -> Float -> Float
withdraw x y 
  | x `mod` 5 == 0 && charge <= y = y - charge
  | otherwise                     = y
  where charge = 0.5 + fromIntegral x

main :: IO ()
main = do
  [sx, sy] <- fmap words getLine
  printf "%.2f" $ withdraw (read sx) (read sy)
