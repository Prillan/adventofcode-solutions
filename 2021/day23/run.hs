{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid
import AoC.Search (dijkstra, dijkstra_)

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.Vector (Vector)
import qualified Data.Vector as V

data Pod = A | B | C | D
  deriving (Read, Show, Eq, Ord, Enum, Generic)

instance Hashable Pod

-- TODO: As with the last couple of days, this really needs some
-- cleanup and performance improvements...

moveCost :: Pod -> Int
moveCost = \case A -> 1
                 B -> 10
                 C -> 100
                 D -> 1000

parse = read @Pod . pure

type Input = [[Pod]]

parseAll :: String -> Input
parseAll =
  map (map parse)
  . transpose
  . filter (not . null)
  . map (filter (`elem` "ABCD"))
  . lines

data Pos = Room {-# UNPACK #-} !Word8  {-# UNPACK #-} !Pod
         | Hallway {-# UNPACK #-} !Word8
  deriving (Show, Eq, Ord, Generic)

instance Hashable Pos

inHallway = \case Hallway _ -> True
                  _         -> False
inRoom = not . inHallway
inRoomT t = \case Room _ t' -> t == t'
                  _         -> False

data Movement = NotMoved | HasMoved | Locked
  deriving (Show, Eq, Ord, Enum)

neighbors :: Pos -> [(Int, Pos)]
neighbors = \case
  Room 0 pod -> [(1, Room 1 pod)]
  Room 1 pod -> [(1, Room 0 pod), (1, Room 2 pod)]
  Room 2 pod -> [(1, Room 1 pod), (1, Room 3 pod)]
  Room 3 A   -> [(1, Room 2 A), (2, Hallway 1), (2, Hallway 3)]
  Room 3 B   -> [(1, Room 2 B), (2, Hallway 3), (2, Hallway 5)]
  Room 3 C   -> [(1, Room 2 C), (2, Hallway 5), (2, Hallway 7)]
  Room 3 D   -> [(1, Room 2 D), (2, Hallway 7), (2, Hallway 9)]
  Hallway 0  -> [(1, Hallway 1)]
  Hallway 1  -> [(1, Hallway 0), (2, Hallway 3), (2, Room 3 A)]
  Hallway 3  -> [(2, Hallway 1), (2, Room 3 A), (2, Room 3 B), (2, Hallway 5)]
  Hallway 5  -> [(2, Hallway 3), (2, Room 3 B), (2, Room 3 C), (2, Hallway 7)]
  Hallway 7  -> [(2, Hallway 5), (2, Room 3 C), (2, Room 3 D), (2, Hallway 9)]
  Hallway 9  -> [(2, Hallway 7), (2, Room 3 D), (1, Hallway 10)]
  Hallway 10 -> [(1, Hallway 9)]

-- TODO: Convert to bfs/dijkstra?
toHallway :: Set Pos -> Pos -> [(Int, Pos)]
toHallway forbidden startPos = go (Set.singleton startPos) [(0, startPos)]
  where go visited = \case
          []      -> []
          ((c, p):ps) ->
            let next = filter (not . vis)
                       . filter (not . forb)
                       . filter valid
                       $ neighbors p
                vis (_, p') = p' `Set.member` visited
                forb (_, p') = p' `Set.member` forbidden
                valid (_, p') =
                  case (p, p') of
                    (Hallway _, Room _ _) -> False
                    (Room i _, Room j _) -> j > i -- should only move to rooms further up
                    _ -> True

                nexts = map (\(c', p') -> (c + c', p')) next
                nextSet = Set.fromList $ map snd nexts
            in filter (inHallway . snd) nexts ++ go (Set.union visited nextSet) (nexts ++ ps)

-- TODO: Convert to bfs/dijkstra?
toRoom :: Set Pos -> Pos -> Pod -> [(Int, Pos)]
toRoom forbidden startPos targetType = go (Set.singleton startPos) [(0, startPos)]
  where go visited = \case
          []      -> []
          ((c, p):ps) ->
            let next = filter (not . vis)
                       . filter (not . forb)
                       . filter valid
                       $ neighbors p
                vis (_, p') = p' `Set.member` visited
                forb (_, p') = p' `Set.member` forbidden
                valid (_, p') =
                  case (p, p') of
                    (Hallway _, Room _ t) -> t == targetType
                    (Room _ _, Hallway _) -> False
                    (Room i _, Room j _) -> j < i -- should only move to rooms further down
                    _ -> True

                nexts = map (\(c', p') -> (c + c', p')) next
                nextSet = Set.fromList $ map snd nexts
            in filter (inRoomT targetType . snd) nexts ++ go (Set.union visited nextSet) (nexts ++ ps)

-- toRoom' :: Set Pos -> Pos -> Pos -> Maybe (Pos, Int)
-- toRoom' forbidden startPos target = dijkstra (== target) n startPos
--   where forb (_, p') = p' `Set.member` forbidden
--         Room _ targetType = target
--         n p =
--           let valid (_, p') =
--                 case (p, p') of
--                   (Hallway _, Room _ t) -> t == targetType
--                   (Room _ _, Hallway _) -> False
--                   (Room i _, Room j _) -> j < i -- should only move to rooms further down
--                   _ -> True
--           in map (\(c, p) -> (p, c))
--              . filter (not . forb)
--              . filter valid
--              $ neighbors p

data State = State { startPods   :: [(Pos, Pod)]
                   , hallwayPods :: [(Pos, Pod)]
                   , donePods    :: [(Pos, Pod)]
                   }
  deriving (Show, Eq, Ord, Generic)

instance Hashable State

allPods State {..} = startPods ++ hallwayPods ++ donePods

roomPods State {..} t = mapMaybe f startPods -- done are guaranteed to be correct
  where f = \case
          (Room _ rt, pt) | t == rt -> Just pt
          _ -> Nothing

done :: State -> Bool
done State {..} = null startPods && null hallwayPods

moveToHallway :: State -> Set Pos -> (Pos, Pod) -> [(State, Int)]
moveToHallway s@State {..} allPos (pos, pod) =
  let next = toHallway (Set.delete pos allPos) pos
      nextStart = delete (pos, pod) startPods
  in map (\(c, pos') -> (s { startPods = nextStart, hallwayPods = (pos', pod):hallwayPods }, c * moveCost pod)) next

moveToRoom depth s@State {..} allPos (pos, pod)
  | all (== pod) (roomPods s pod) =
    -- sort is a hack, will pick the one with the lowest room number,
    -- which is 0 if it's available, otherwise 1.
    let -- targetPos = case sort (filter (\case Room _ rt -> rt == pod) $ map fst donePods) of
        --               []          -> Room (4 - depth) pod
        --               Room d _:_  -> Room (d + 1) pod
        next = take 1
               . sortBy (comparing snd)
               . filter (\(_, Room i _) -> i >= 4 - depth)
               $ toRoom allPos pos pod

        --next = toRoom allPos pos pod
        nextHW = delete (pos, pod) hallwayPods
    in map (\(c, pos') -> (s { hallwayPods = nextHW, donePods = (pos', pod):donePods }, c * moveCost pod)) next
  | otherwise = []

nexts :: Word8 -> State -> [(State, Int)]
nexts depth s@State {..} =
  let allPos = Set.fromList $ map fst $ allPods s
      toH = concatMap (moveToHallway s allPos) startPods
      toR = concatMap (moveToRoom depth s allPos) hallwayPods
  in toH ++ toR


-- For testing
fakeInit = State { startPods = [(Room 2 A, A), (Room 3 A, B)]
                 , hallwayPods = []
                 , donePods = []
                 }

-- Makes sure that any done are moved to done before we start,
-- actually only relevant in the example, I believe.
setup :: Word8 -> State -> State
setup depth s@State {..} = let (startPods', assigned) = foldl f (startPods, []) [0..3]
                     in s { startPods = startPods'
                          , donePods = map (\pos@(Room _ t) -> (pos, t)) assigned }
  where f (pods, cs) i =
          let (c0, rest) = partition (correct i cs) pods
          in (rest, cs ++ map fst c0)
        correct d assigned = \case
          (Room d' t, t') -> d == d' && t == t' && (d == (4 - depth) || Room (d - 1) t `elem` assigned)
          _               -> False


-- TODO: Use A* with a nice heuristic?
solve :: Word8 -> State -> Int
solve depth = (\case Just v -> v) . dijkstra_ done (nexts depth)

toState :: Input -> State
toState input = State (concat $ zipWith f input [A .. D]) [] []
  where f xs rt = zipWith (\pt i -> (Room i rt, pt)) xs [3,2,1,0]

part1 = solve 2 . setup 2 . toState

--  #D#C#B#A#
--  #D#B#A#C#
injectFold = zipWith (\zs [x, y] -> x:zs ++ [y]) folded
  where folded = [ [D, D]
                 , [C, B]
                 , [B, A]
                 , [A, C]
                 ]

part2 = solve 4 . setup 4 . toState . injectFold

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
