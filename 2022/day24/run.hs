{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
import AoC.Grid (parseMapGrid)
import AoC.Search (astar_)

import Data.Foldable (toList)
import Data.List.Split
import Data.Maybe (mapMaybe)

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set

-- TODO: Should be able to optimize this a bit more, runs in 9-10 s
-- with a lot of memory usage.

type N = Int

data Dir = N | S | W | E
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable Dir

next :: (Eq a, Enum a, Bounded a) => a -> a
next d | d == maxBound = minBound
       | otherwise     = succ d

data Cell = Empty
          | Wall
          | Blizzards (Set Dir)
  deriving Eq

instance Show Cell where
  show =
    \case Empty -> "."
          Wall -> "#"
          Blizzards xs ->
            case toList xs of
              [E] -> ">"
              [S] -> "v"
              [N] -> "^"
              [W] -> "<"
              _   -> show (length xs)

isBlizzard :: Cell -> Bool
isBlizzard = \case Blizzards _ -> True
                   _ -> False

isEmpty :: Cell -> Bool
isEmpty = \case Empty -> True
                _ -> False

cell :: Char -> Cell
cell = \case '.' -> Empty
             '#' -> Wall
             '>' -> Blizzards (Set.singleton E)
             'v' -> Blizzards (Set.singleton S)
             '^' -> Blizzards (Set.singleton N)
             '<' -> Blizzards (Set.singleton W)

parseAll = parseMapGrid cell

getBounds g =
  let (cs, rs) = unzip $ HashMap.keys g
  in ((minimum cs, minimum rs), (maximum cs, maximum rs))

start :: (N, N)
start = (0, 0)

type Pos = (N, N)

hv :: Pos -> [Pos]
hv (c, r) =
  [ (c    , r + 1)
  , (c    , r - 1)
  , (c + 1, r    )
  , (c - 1, r    )
  ]

hvSame :: Pos -> [Pos]
hvSame p = p:hv p

reverseDir :: Dir -> Dir
reverseDir = \case N -> S
                   S -> N
                   W -> E
                   E -> W

--hvNeighbors :: MapGrid N -> Pos -> [(Pos, Int)]
hvNeighbors m =
  mapMaybe (\pos -> sequence (pos, pos `HashMap.lookup` m))
  . hv

hvSameNeighbors :: HashMap Pos Cell -> Pos -> [(Pos, Cell)]
hvSameNeighbors m =
  mapMaybe (\pos -> sequence (pos, pos `HashMap.lookup` m))
  . hvSame
{-# INLINE hvSameNeighbors #-}

wrap1 :: N -> N -> N
wrap1 l v =
  ((l - 1 + v) `mod` l) + 1
{-# INLINE wrap1 #-}

wrap :: ((N, N), (N, N)) -> Pos -> Pos
wrap b@((xmin, ymin), (xmax, ymax)) (c, r) =
  let xl = xmax - xmin - 1
      yl = ymax - ymin - 1
  in
    (wrap1 xl c, wrap1 yl r)
{-# INLINE wrap #-}

blizzardStep :: ((N, N), (N, N)) -> (N, N) -> Dir -> Pos
blizzardStep b (c, r) d =
  let p' = case d of
             N -> (c, r - 1)
             S -> (c, r + 1)
             E -> (c + 1, r)
             W -> (c - 1, r)
  in wrap b p'
{-# INLINE blizzardStep #-}

stepBlizzards :: HashMap Pos Cell -> ((N, N), (N, N)) -> HashMap Pos Cell
stepBlizzards g b =
  let empty = HashMap.map (\case Wall -> Wall
                                 _    -> Empty) g

      f :: Pos -> Cell -> [(Pos, Cell)]
      f p (Blizzards xs) =
        map (\d -> (blizzardStep b p d, Blizzards (Set.singleton d)))
        $ Set.toList xs

      merge v1 v2 =
        case (v1, v2) of
          (Wall, _)  -> error "WALL!"
          (_, Wall)  -> error "WALL!"
          (Empty, x) -> x
          (x, Empty) -> x
          (Blizzards b1, Blizzards b2) -> Blizzards (Set.union b1 b2)
  in HashMap.unionWith merge empty
     . HashMap.fromListWith merge
     . concatMap (uncurry f)
     . HashMap.toList
     $ HashMap.filter isBlizzard g

moves :: HashMap Pos Cell
      -> ((N, N), (N, N))
      -> Pos
      -> [Pos]
moves g b p =
  map fst
  . filter (\(p', c) -> isEmpty c)
  $ hvSameNeighbors g p

path :: HashMap (N, N) Cell
     -> ((N, N), (N, N))
     -> (N, N)
     -> (N, N)
     -> Maybe N
path g b start end@(ec, er) =
  let blizzards = iterate (flip stepBlizzards b) g

      neighbors :: (Int, Pos) -> [((Int, Pos), N)]
      neighbors (!t, !p) =
        map (\p' -> ((t + 1, p'), 1))
        $ moves (blizzards !! (t + 1)) b p
      h (t, (!c, !r))
        | t >= 10000 = error "ASDFASDF"
        | otherwise = abs (c - ec) + abs (r - er)
  in astar_ (\(_, p) -> p == end) neighbors h (0, start)

part1 :: HashMap (N, N) Cell -> N
part1 g =
  let b@(_, (xmax, ymax)) = getBounds g
      Just answer = path g b (1, 0) (xmax - 1, ymax)
  in answer

path' :: HashMap (N, N) Cell
      -> ((N, N), (N, N))
      -> (N, N)
      -> (N, N)
      -> Maybe N
path' g b start@(sc, sr) end@(ec, er) =
  let blizzards = chunksOf 30 $ iterate (flip stepBlizzards b) g
      lookup t =
        let (ci, i) = t `divMod` 30
        in (blizzards !! ci) !! i
      {-# INLINE lookup #-}
      sediff = abs (sc - ec) + abs (sr - er)

      neighbors (_, _, []) = []
      neighbors (!t, !p, target:rest) =
        map (\case p'
                     | p' == target -> ((t + 1, p', rest), 1)
                     | otherwise -> ((t + 1, p', target:rest), 1))
        $ moves (lookup (t + 1)) b p

      h (_, _, []) = 0
      h (t, (!c, !r), (tc, tr):rest)
        | t >= 10000 = error "ASDFASDF"
        | otherwise = abs (c - tc) + abs (r - tr) + sediff * length rest
  in astar_ (\case (_, _, []) -> True
                   _ -> False) neighbors h (0, start, [end, start, end])

part2 :: HashMap (N, N) Cell -> N
part2 g =
  let b@(_, (xmax, ymax)) = getBounds g
      Just answer = path' g b (1, 0) (xmax - 1, ymax)
  in answer

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example2.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
