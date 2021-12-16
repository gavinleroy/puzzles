{-# LANGUAGE BangPatterns #-}

-- Gavin Gray AOC 21

module Main where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Functor ((<&>))
import Data.List (foldl', foldl1')
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

type Ver  = Int
type Ty   = Int
data Bits = Literal Integer
          | Operator Ty [Integer]
  deriving (Show)

-- FIXME there must be a better way to do this
hexToInteger :: Char -> String
hexToInteger '0' = "0000"
hexToInteger '1' = "0001"
hexToInteger '2' = "0010"
hexToInteger '3' = "0011"
hexToInteger '4' = "0100"
hexToInteger '5' = "0101"
hexToInteger '6' = "0110"
hexToInteger '7' = "0111"
hexToInteger '8' = "1000"
hexToInteger '9' = "1001"
hexToInteger 'A' = "1010"
hexToInteger 'B' = "1011"
hexToInteger 'C' = "1100"
hexToInteger 'D' = "1101"
hexToInteger 'E' = "1110"
hexToInteger 'F' = "1111"
hexToInteger _   = []

hexToBin :: String -> String
hexToBin = (hexToInteger =<<)

bintodec :: Integral i => i -> i
bintodec 0 = 0
bintodec i = (\x -> 2 * x + last) (bintodec (div i 10))
    where last = mod i 10

fixed :: Int -> Parser Int
fixed n = bintodec . read <$> count n digit

typ :: Parser Ty
typ = fixed 3

version :: Parser Ver
version = fixed 3

single :: Parser String
single = count 1 digit

parseLiteral :: Parser Integer
parseLiteral = loop ""
  where
    loop :: String -> Parser Integer
    loop acc = do
      c <- single
      d <- count 4 digit
      let inter = acc ++ d
      if c == "0" then
        return . bintodec $ read inter
      else loop inter

parseInner :: Parser [Integer]
parseInner = do
  c <- single
  if c == "0" then
    (fixed 15 >>= flip count digit)
    <&> runParsec
  else fixed 11 >>= flip count parseBit

parseBit :: Parser Integer
parseBit = do
  _   <- version
  ty  <- typ
  if ty == 4 then
    eval . Literal <$> parseLiteral
  else do
    eval . Operator ty <$> parseInner

-- FIXME this is terrible
parseBits :: Parser [Integer]
parseBits =
  try (do bit <- parseBit
          fmap (bit : ) parseBits)
  <|> return []

runParsec :: String -> [Integer]
runParsec input =
  case parse parseBits "" input of
    Right e -> e

runParsecSingle :: String -> Integer
runParsecSingle input =
  case parse parseBit "" input of
    Right e -> e

eval :: Bits -> Integer
eval (Literal !n) = n
eval (Operator t !bs)
  | t == 0 = foldl' (+) 0 bs
  | t == 1 = foldl' (*) 1 bs
  | t == 2 = foldl1' min bs
  | t == 3 = foldl1' max bs
  | t == 5 = if apply' (>)  bs then 1 else 0
  | t == 6 = if apply' (<)  bs then 1 else 0
  | t == 7 = if apply' (==) bs then 1 else 0
  where apply' a [!f, !s] = a f s

main :: IO ()
main = interact $ show . runParsecSingle . hexToBin
