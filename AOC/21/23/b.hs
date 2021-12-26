-- Gavin Gray AOC 21

-- NOTE FIXME what a terrible solution
--            come back later and cleanup

module Main where

import Algorithm.Search  (dijkstra, bfs)
import Data.Function (on)
import qualified Data.List     as L
import Data.List.Split (chunksOf)
import Data.Maybe (isJust, fromMaybe, maybeToList)
import qualified Data.Vector   as V

type Pos = Int
type Cost = Int
type Weight = Int
type Space = (RoomType, Pos)
data Member = A | B | C | D
  deriving (Eq, Show, Ord)
data RoomType = Hallway | Room Member
  deriving (Eq, Show)
type Amphipod = (Member, Pos)
type Board = V.Vector [Space]
type State = [Amphipod]

sshow :: State -> String
sshow s = unlines
      ["|-----------|"
      ,"|" ++ sa 0 ++ sa 1 ++ "-" ++ sa 2 ++ "-" ++ sa 3 ++ "-" ++ sa 4 ++ "-" ++ sa 5 ++ sa 6 ++ "|"
      ,"|__" ++ sa 7 ++ "|" ++ sa 11 ++ "|" ++ sa 15 ++ "|" ++ sa 19 ++ "__|"
      ,"  |" ++ sa 8 ++ "|" ++ sa 12 ++ "|" ++ sa 16 ++ "|" ++ sa 20 ++ "|  "
      ,"  |" ++ sa 9 ++ "|" ++ sa 13 ++ "|" ++ sa 17 ++ "|" ++ sa 21 ++ "|  "
      ,"  |" ++ sa 10++ "|" ++ sa 14 ++ "|" ++ sa 18 ++ "|" ++ sa 22 ++ "|  "
      ,"  |_______|  "]
  where
    sa idx = maybe "." (show . fst)
      (L.find ((==idx) . snd) s)

board :: Board
board = V.fromList
  [[(Hallway, 1)] -- Hallway positions
  ,[(Hallway, 0), (Hallway, 2), (Room A, 7)]
  ,[(Hallway, 1), (Hallway, 3), (Room A, 7),  (Room B, 11)]
  ,[(Hallway, 2), (Hallway, 4), (Room B, 11), (Room C, 15)]
  ,[(Hallway, 3), (Hallway, 5), (Room C, 15), (Room D, 19)]
  ,[(Hallway, 6), (Hallway, 4), (Room D, 19)]
  ,[(Hallway, 5)]
  --
  ,[(Room A, 8), (Hallway, 2), (Hallway, 1)]
  ,[(Room A, 7), (Room A, 9)]
  ,[(Room A, 8), (Room A, 10)]
  ,[(Room A, 9)]
  --
  ,[(Room B, 12), (Hallway, 2), (Hallway, 3)]
  ,[(Room B, 11), (Room B, 13)]
  ,[(Room B, 12), (Room B, 14)]
  ,[(Room B, 13)]
  --
  ,[(Room C, 16), (Hallway, 3), (Hallway, 4)]
  ,[(Room C, 15), (Room C, 17)]
  ,[(Room C, 16), (Room C, 18)]
  ,[(Room C, 17)]
  --
  ,[(Room D, 20), (Hallway, 4), (Hallway, 5)]
  ,[(Room D, 19), (Room D, 21)]
  ,[(Room D, 20), (Room D, 22)]
  ,[(Room D, 21)]]

cost'matrix :: V.Vector (V.Vector Cost)
cost'matrix = V.fromList
  [V.fromList [ 0,  1,  3,  5,  7,  9, 10,  3,  4,  5,  6,  5,  6,  7,  8,  7,  8,  9, 10,  9, 10, 11, 12]
  ,V.fromList [ 1,  0,  2,  4,  6,  8,  9,  2,  3,  4,  5,  4,  5,  6,  7,  6,  7,  8,  9,  8,  9, 10, 11]
  ,V.fromList [ 3,  2,  0,  2,  4,  6,  7,  2,  3,  4,  5,  2,  3,  4,  5,  4,  5,  6,  7,  6,  7,  8,  9]
  ,V.fromList [ 5,  4,  2,  0,  2,  4,  5,  4,  5,  6,  7,  2,  3,  4,  5,  2,  3,  4,  5,  4,  5,  6,  7]
  ,V.fromList [ 7,  6,  4,  2,  0,  2,  3,  6,  7,  8,  9,  4,  5,  6,  7,  2,  3,  4,  5,  2,  3,  4,  5]
  ,V.fromList [ 9,  8,  6,  4,  2,  0,  1,  8,  9, 10, 11,  6,  7,  8,  9,  4,  5,  6,  7,  2,  3,  4,  5]
  ,V.fromList [10,  9,  7,  5,  3,  1,  0,  9, 10, 11, 12,  7,  8,  9, 10,  5,  6,  7,  8,  3,  4,  5,  6]
  -- Room Positions
  ,V.fromList [ 3,  2,  2,  4,  6,  8,  9,  0,  1,  2,  3,  4,  5,  6,  7,  6,  7,  8,  9,  8,  9, 10, 11]
  ,V.fromList [ 4,  3,  3,  5,  7,  9, 10,  1,  0,  1,  2,  5,  6,  7,  8,  7,  8,  9, 10,  9, 10, 11, 12]
  ,V.fromList [ 5,  4,  4,  6,  8, 10, 11,  2,  1,  0,  1,  6,  7,  8,  9,  8,  9, 10, 11, 10, 11, 12, 13]
  ,V.fromList [ 6,  5,  5,  7,  9, 11, 12,  3,  2,  1,  0,  7,  8,  9, 10,  9, 10, 11, 12, 11, 12, 13, 14]

  ,V.fromList [ 5,  4,  2,  2,  4,  6,  7,  4,  5,  6,  7,  0,  1,  2,  3,  4,  5,  6,  7,  6,  7,  8,  9]
  ,V.fromList [ 6,  5,  3,  3,  5,  7,  8,  5,  6,  7,  8,  1,  0,  1,  2,  5,  6,  7,  8,  7,  8,  9, 10]
  ,V.fromList [ 7,  6,  4,  4,  6,  8,  9,  6,  7,  8,  9,  2,  1,  0,  1,  6,  7,  8,  9,  8,  9, 10, 11]
  ,V.fromList [ 8,  7,  5,  5,  7,  9, 10,  7,  8,  9, 10,  3,  2,  1,  0,  7,  8,  9, 10,  9, 10, 11, 12]

  ,V.fromList [ 7,  6,  4,  2,  2,  4,  5,  6,  7,  8,  9,  4,  5,  6,  7,  0,  1,  2,  3,  4,  5,  6,  7]
  ,V.fromList [ 8,  7,  5,  3,  3,  5,  6,  7,  8,  9, 10,  5,  6,  7,  8,  1,  0,  1,  2,  5,  6,  7,  8]
  ,V.fromList [ 9,  8,  6,  4,  4,  6,  7,  8,  9, 10, 11,  6,  7,  8,  9,  2,  1,  0,  1,  6,  7,  8,  9]
  ,V.fromList [10,  9,  7,  5,  5,  7,  8,  9, 10, 11, 12,  7,  8,  9, 10,  3,  2,  1,  0,  7,  8,  9, 10]

  ,V.fromList [ 9,  8,  6,  4,  2,  2,  3,  8,  9, 10, 11,  6,  7,  8,  9,  4,  5,  6,  7,  0,  1,  2,  3]
  ,V.fromList [10,  9,  7,  5,  3,  3,  4,  9, 10, 11, 12,  7,  8,  9, 10,  5,  6,  7,  8,  1,  0,  1,  2]
  ,V.fromList [11, 10,  8,  6,  4,  4,  5, 10, 11, 12, 13,  8,  9, 10, 11,  6,  7,  8,  9,  2,  1,  0,  1]
  ,V.fromList [12, 11,  9,  7,  5,  5,  6, 11, 12, 13, 14,  9, 10, 11, 12,  7,  8,  9, 10,  3,  2,  1,  0]]

energy :: Amphipod -> Weight -> Cost
energy (A,_) = (*1)
energy (B,_) = (*10)
energy (C,_) = (*100)
energy (D,_) = (*1000)

room'occupants :: [Amphipod] -> Member -> [Amphipod]
room'occupants as A = filter (\(_,p) ->  7 <= p && p <= 10) as
room'occupants as B = filter (\(_,p) -> 11 <= p && p <= 14) as
room'occupants as C = filter (\(_,p) -> 15 <= p && p <= 18) as
room'occupants as D = filter (\(_,p) -> 19 <= p && p <= 22) as

in'right'room :: Amphipod -> Bool
in'right'room (A, p) =  7 <= p && p <= 10
in'right'room (B, p) = 11 <= p && p <= 14
in'right'room (C, p) = 15 <= p && p <= 18
in'right'room (D, p) = 19 <= p && p <= 22

-- NOTE reverse the list to make searching
--      for the lowest position faster
rooms'of :: Member -> [Space]
rooms'of A = L.reverse [(Room A, 7), (Room A, 8), (Room A, 9), (Room A,10)]
rooms'of B = L.reverse [(Room B,11), (Room B,12), (Room B,13), (Room B,14)]
rooms'of C = L.reverse [(Room C,15), (Room C,16), (Room C,17), (Room C,18)]
rooms'of D = L.reverse [(Room D,19), (Room D,20), (Room D,21), (Room D,22)]

in'hallway :: Pos -> Bool
in'hallway p = 0 <= p && p <= 6

in'room :: Pos -> Bool
in'room = not . in'hallway

stay'in'room :: [Amphipod] -> Amphipod -> Bool
stay'in'room others a@(m, pos) =
  in'right'room a &&
  all (\(mb, p) -> mb == m || p < pos) (room'occupants others m)

open'pos :: [Amphipod] -> Pos -> [Space]
open'pos others p = filter (not . occupied others) (V.unsafeIndex board p)
  where
    occupied as (_, p) = any ((p==) . snd) as

into'hallway :: [Amphipod] -> Amphipod -> [Amphipod]
into'hallway others (m, src) =
  [ (m, hp) | hp <- hallways,
    isJust $! bfs (map snd . open'pos others) (==hp) src ]
  where hallways = [0..6]

-- NOTE choose the lowest  point in the room if you can enter
into'room :: [Amphipod] -> Amphipod -> [Amphipod]
into'room others (m, src)
  | not $ all in'right'room (room'occupants others m) = []
  | otherwise = fmap (\(Room _, p) -> (m, p)) $ maybeToList
    $ L.find (\(_, rp) -> isJust $ bfs (map snd . open'pos others) (==rp) src)
      (rooms'of m)

move'one :: [Amphipod] -> Amphipod -> [Amphipod]
move'one others am@(_, pos)
  | stay'in'room others am = []
  | in'hallway pos = into'room others am
  | in'room pos    = into'hallway others am

cost'from'to :: State -> State -> Cost
cost'from'to s1 s2 = cost'f't from to
  where
    ([from], [to]) = (s1 L.\\ s2, s2 L.\\ s1)
    cost'f't :: Amphipod -> Amphipod -> Cost
    cost'f't a@(_, p1) (_, p2) =
      energy a (flip V.unsafeIndex p2 $ V.unsafeIndex cost'matrix p1)


states :: State -> [State]
states ams = concatMap
  (\a -> let oths = L.delete a ams in
           L.sort $! map (:oths)
           $! move'one oths a) ams

is'correct :: State -> Bool
is'correct = all in'right'room

parse :: String -> State
parse s = concat . zipWith (zipWith makea) idxs
  . L.transpose . map removec . drop 2 . lines $ s
  where
    makea i 'A' = (A, i)
    makea i 'B' = (B, i)
    makea i 'C' = (C, i)
    makea i 'D' = (D, i)
    idxs = chunksOf size [7..]
    size = (length $ lines s) - 3
    removec [] = []
    removec ('#' :  tl) = removec tl
    removec (' ' :  tl) = removec tl
    removec (l : ls) = l : removec ls

main :: IO ()
main = interact $ show . fst . fromMaybe (-1, [])
  . dijkstra states cost'from'to is'correct . parse
