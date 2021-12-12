-- Gavin Gray AOC 21

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isUpper)
import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Text as T

data Vertex = Start | End | Large String | Small String
  deriving (Eq, Ord)
type Edge   = (Vertex, Vertex)
type Graph  = M.Map String [Vertex]

instance Show Vertex where
  show Start     = "start"
  show End       = "end"
  show (Large s) = s
  show (Small s) = s

-- Parsing Graph

uppercase :: String -> Bool
uppercase = all isUpper

parseVertex :: String -> Vertex
parseVertex "start"         = Start
parseVertex "end"           = End
parseVertex s | uppercase s = Large s
              | otherwise   = Small s

parseEdge :: [String] -> Edge
parseEdge (v1 : v2 : _) = (parseVertex v1, parseVertex v2)
parseEdge _             = error "Impossible"

-- FIXME I should be faster :)
insertEdge :: Graph -> Edge -> Graph
insertEdge g (v1, v2) = M.insertWith (++) (show v1) [v2]
                      $ M.insertWith (++) (show v2) [v1] g

-- FIXME I shouldn't be unpacking the Text
buildGraph :: String -> Graph
buildGraph = foldl insertEdge M.empty
           . map (parseEdge . map T.unpack . T.splitOn "-")
           . map T.pack
           . lines

-- Logic

adjacent :: Graph -> Vertex -> [Vertex]
adjacent g v = g M.! (show v)

canVisit :: S.Set Vertex -> Vertex -> Bool
canVisit s v@(Small _) = S.notMember v s
canVisit _ Start       = False
canVisit _ _           = True

findPaths :: Graph -> S.Set Vertex -> Vertex -> Vertex -> Int
findPaths g visited src tgt
  | src == tgt = 1
  | otherwise  = sum
               $ map (flip (findPaths g vs') tgt)
               $ filter (canVisit vs')
               $ adjacent g src
  where vs' = S.insert src visited

countPaths :: Graph -> Int
countPaths g = findPaths g S.empty Start End

main :: IO ()
main = interact $ show . countPaths . buildGraph
