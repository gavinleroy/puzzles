-- Gavin Gray AOC 21

module Main where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.List (tails)
import Data.Function (fix, on)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

data Number = Many Number Number
            | Sing Int
  deriving (Show)

-- Parsing

single :: Parser Number
single = Sing . read <$> many1 digit

many' :: Parser Number
many' = do
  char '['
  f <- num
  char ','
  s <- num
  char ']'
  return (Many f s)

num :: Parser Number
num = single <|> many'

parseLine :: String -> Number
parseLine inp = case parse num "" inp of
  Right e -> e

-- Logic

idiv :: Int -> Int -> Float
idiv = (/) `on` fromIntegral

divu :: Int -> Int -> Int
divu l r = ceiling (idiv l r)

divd :: Int -> Int -> Int
divd l r = floor (idiv l r)

add :: Number -> Number -> Number
add f s = reduce (Many f s)

goleft :: Maybe Int -> Number -> Number
goleft Nothing n = n
goleft (Just v) (Sing n) = Sing (n + v)
goleft v (Many l r) = Many (goleft v l) r

goright :: Maybe Int -> Number -> Number
goright Nothing n = n
goright (Just v) (Sing n) = Sing (n + v)
goright v (Many l r) = Many l (goright v r)

(!+) :: Int -> Int -> Int
(!+) l r = if l + r > 4 then 4 else l + r

explode :: Number -> Either Number Number
explode n = case exp n 0 of
  Left n -> Left n
  Right (_, n, _) -> Right n
  where exp (Sing n) _ = Left (Sing n)
        exp (Many (Sing l) (Sing r)) 4 =
             Right (Just l, Sing 0, Just r)
        exp (Many l r) d = case exp l (d !+ 1) of
          Right (lv, l, rv) ->
            Right (lv, Many l (goleft rv r), Nothing)
          Left n -> case exp r (d !+ 1) of
            Right (lv, r, rv) ->
              Right (Nothing, Many (goright lv l) r, rv)
            Left n -> Left (Many l n)

split :: Number -> Either Number Number
split (Sing n)
  | n > 9 = Right (Many (Sing (n `divd` 2))  (Sing (n `divu` 2)))
  | otherwise = Left (Sing n)
split (Many l r) = case split l of
  Right l -> Right (Many l r)
  Left _ -> case split r of
    Right r -> Right (Many l r)
    Left _ -> Left (Many l r)

magnitude :: Number -> Int
magnitude (Sing n) = n
magnitude (Many f s) =
  3 * magnitude f + 2 * magnitude s

reduce :: Number -> Number
reduce n = case explode n of
  Right n -> reduce n
  Left n -> case split n of
    Right n -> reduce n
    Left n -> n

solve :: [Number] -> Int
solve l = maximum
  $  map f $ join $ [[(x, y), (y, x)] | (x:ys) <- tails l, y <- ys]
  where f = magnitude . uncurry add

main :: IO ()
main = interact $ show . solve . map parseLine . lines
