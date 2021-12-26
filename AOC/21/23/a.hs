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
      ,"|__" ++ sa 7 ++ "|" ++ sa 9 ++ "|" ++ sa 11 ++ "|" ++ sa 13 ++ "__|"
      ,"  |" ++ sa 8 ++ "|" ++ sa 10 ++ "|" ++ sa 12 ++ "|" ++ sa 14 ++ "|  "
      ,"  |_______|  "]
  where
    sa idx = maybe "." (show . fst)
      (L.find ((==idx) . snd) s)

board :: Board
board = V.fromList
  [[(Hallway, 1)] -- Hallway positions
  ,[(Hallway, 0), (Hallway, 2), (Room A, 7)]
  ,[(Hallway, 1), (Hallway, 3), (Room A, 7), (Room B, 9)]
  ,[(Hallway, 2), (Hallway, 4), (Room B, 9), (Room C, 11)]
  ,[(Hallway, 3), (Hallway, 5), (Room C, 11), (Room D, 13)]
  ,[(Hallway, 6), (Hallway, 4), (Room D, 13)]
  ,[(Hallway, 5)]
  ,[(Room A, 8),  (Hallway, 2), (Hallway, 1)]
  ,[(Room A, 7)]
  ,[(Room B, 10), (Hallway, 2), (Hallway, 3)]
  ,[(Room B, 9)]
  ,[(Room C, 12), (Hallway, 3), (Hallway, 4)]
  ,[(Room C, 11)]
  ,[(Room D, 14), (Hallway, 4), (Hallway, 5)]
  ,[(Room D, 13)]]

cost'matrix :: V.Vector (V.Vector Cost)
cost'matrix = V.fromList
  [V.fromList [ 0,  1,  3,  5,  7,  9, 10,  3,  4,  5,  6,  7,  8,  9, 10]
  ,V.fromList [ 1,  0,  2,  4,  6,  8,  9,  2,  3,  4,  5,  6,  7,  8,  9]
  ,V.fromList [ 3,  2,  0,  2,  4,  6,  7,  2,  3,  2,  3,  4,  5,  6,  7]
  ,V.fromList [ 5,  4,  2,  0,  2,  4,  5,  4,  5,  2,  3,  2,  3,  4,  5]
  ,V.fromList [ 7,  6,  4,  2,  0,  2,  3,  6,  7,  4,  5,  2,  3,  2,  3]
  ,V.fromList [ 9,  8,  6,  4,  2,  0,  1,  8,  9,  6,  7,  4,  5,  2,  3]
  ,V.fromList [10,  9,  7,  5,  3,  1,  0,  9, 10,  7,  8,  5,  6,  3,  4]
  -- Room Positions
  ,V.fromList [ 3,  2,  2,  4,  6,  8,  9,  0,  1,  4,  5,  6,  7,  8,  9]
  ,V.fromList [ 4,  3,  3,  5,  7,  9, 10,  1,  0,  5,  6,  7,  8,  9, 10]
  ,V.fromList [ 5,  4,  2,  2,  4,  6,  7,  4,  5,  0,  1,  4,  5,  6,  7]
  ,V.fromList [ 6,  5,  3,  3,  5,  7,  8,  5,  6,  1,  0,  5,  6,  7,  8]
  ,V.fromList [ 7,  6,  4,  2,  2,  4,  5,  6,  7,  4,  5,  0,  1,  4,  5]
  ,V.fromList [ 8,  7,  5,  3,  3,  5,  6,  7,  8,  5,  6,  1,  0,  5,  6]
  ,V.fromList [ 9,  8,  6,  4,  2,  2,  3,  8,  9,  6,  7,  4,  5,  0,  1]
  ,V.fromList [10,  9,  7,  5,  3,  3,  4,  9, 10,  7,  8,  5,  6,  1,  0]]

energy :: Amphipod -> Weight -> Cost
energy (A,_) = (*1)
energy (B,_) = (*10)
energy (C,_) = (*100)
energy (D,_) = (*1000)

room'occupants :: [Amphipod] -> Member -> [Amphipod]
room'occupants as A = filter (\(_,p) ->  7 <= p && p <=  8) as
room'occupants as B = filter (\(_,p) ->  9 <= p && p <= 10) as
room'occupants as C = filter (\(_,p) -> 11 <= p && p <= 12) as
room'occupants as D = filter (\(_,p) -> 13 <= p && p <= 14) as

in'right'room :: Amphipod -> Bool
in'right'room (A, p) =  7 <= p && p <=  8
in'right'room (B, p) =  9 <= p && p <= 10
in'right'room (C, p) = 11 <= p && p <= 12
in'right'room (D, p) = 13 <= p && p <= 14

-- NOTE reverse the list to make searching
--      for the lowest position faster
rooms'of :: Member -> [Space]
rooms'of A = L.reverse [(Room A, 7), (Room A, 8)]
rooms'of B = L.reverse [(Room B, 9), (Room B,10)]
rooms'of C = L.reverse [(Room C,11), (Room C,12)]
rooms'of D = L.reverse [(Room D,13), (Room D,14)]

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
cost'from'to s1 s2 =
  if cost'f't to from /= cost'f't from to then
    error ("Impossible" ++ unlines ["", show to, show from])
  else cost'f't from to
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
