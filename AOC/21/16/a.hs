-- Gavin Gray AOC 21

module Main where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

type Ver  = Int
type Ty   = Int
data Bits = Literal Ver Ty Int
          | Operator Ver Ty [Bits]
  deriving (Show)

-- FIXME there must be a better way to do this
hexToInt :: Char -> String
hexToInt '0' = "0000"
hexToInt '1' = "0001"
hexToInt '2' = "0010"
hexToInt '3' = "0011"
hexToInt '4' = "0100"
hexToInt '5' = "0101"
hexToInt '6' = "0110"
hexToInt '7' = "0111"
hexToInt '8' = "1000"
hexToInt '9' = "1001"
hexToInt 'A' = "1010"
hexToInt 'B' = "1011"
hexToInt 'C' = "1100"
hexToInt 'D' = "1101"
hexToInt 'E' = "1110"
hexToInt 'F' = "1111"
hexToInt _   = []

hexToBin :: String -> String
hexToBin = join . fmap hexToInt

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

parseLiteral :: Parser Int
parseLiteral = loop ""
  where
    loop :: String -> Parser Int
    loop acc = do
      c <- single
      d <- count 4 digit
      let inter = acc ++ d
      if c == "0" then
        return . bintodec $ read inter
      else loop inter

parseInner :: Parser [Bits]
parseInner = do
  c <- single
  if c == "0" then
    fixed 15
    >>= flip count digit
    >>= return . runParsec
  else fixed 11 >>= flip count parseBit

parseBit :: Parser Bits
parseBit = do
  ver <- version
  ty  <- typ
  if ty == 4 then
    Literal ver ty <$> parseLiteral
  else do
    Operator ver ty <$> parseInner

-- FIXME this is terrible
parseBits :: Parser [Bits]
parseBits = try (do bit <- parseBit
                    fmap (bit:) parseBits)
  <|> return []

runParsec :: String -> [Bits]
runParsec input = case parse parseBits "" input of
  Left e -> error $ show e
  Right e -> e

sumvs :: [Bits] -> Int
sumvs = sum . map sumv
  where
    sumv :: Bits -> Int
    sumv (Literal v _ _) = v
    sumv (Operator v _ bs) = v + sumvs bs

main :: IO ()
main = interact $ show . sumvs . runParsec . hexToBin
