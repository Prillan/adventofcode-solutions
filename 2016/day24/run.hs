{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
import AoC.Search

import           Control.Monad (guard)
import           Data.Bifunctor (first, second)
import           Data.List (permutations, sort, minimum)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue
import           Data.Set (Set)
import qualified Data.Set as Set

unsafeRight (Right x) = x

data Cell = Wall
          | Open
          | Spot Int
  deriving (Show, Eq)

cellNum (Spot x) = pure x
cellNum _ = fail ""

wall Wall = True
wall _ = False

readCell '#' = Wall
readCell '.' = Open
readCell x = Spot (read [x])

parseAll :: String -> HashMap (Int, Int) Cell
parseAll = HashMap.fromList . concat . zipWith parseLine [0..] . lines
  where parseLine y line = zipWith (\x c -> ((x, y), readCell c)) [0..] line

type State = (Int, Int)

neighboursOf :: HashMap (Int, Int) Cell -> (Int, Int) -> [(Int, Int)]
neighboursOf m (x, y) = do
  (x', y') <- [(x, y-1), (x-1, y), (x, y+1), (x+1, y)]
  Just cell <- pure $ HashMap.lookup (x', y') m
  guard $ not (wall cell)
  pure (x', y')

find :: Eq v => v -> HashMap k v -> [k]
find v = map fst . filter ((== v) . snd) . HashMap.toList

d :: (Int, Int) -> (Int, Int) -> Int
d (x0, y0) (x1, y1) =
  abs (x1 - x0) + abs (y1 - y0)

graphify :: HashMap (Int, Int) Cell -> ([Int], HashMap (Int, Int) Int)
graphify m =
  let numbers = sort . concatMap (cellNum.snd) . HashMap.toList $ m
  in
    (numbers, HashMap.fromList $ do
      i0 <- numbers
      i1 <- numbers
      guard $ i0 < i1
      pos0 <- find (Spot i0) m
      pos1 <- find (Spot i1) m
      let Just steps = astar_ (pos1 ==)
                              (map (,1) . neighboursOf m)
                              (d pos1)
                              pos0
      pure $ ((i0, i1), steps))


ts :: [Int] -> HashMap (Int, Int) Int -> Bool -> Int
ts (start:numbers) graph addZero =
  minimum $ do
    permuted <- permutations numbers
    let sequence = permuted ++ (if addZero then [0] else [])
    pure $ sum $ zipWith dist (start:sequence) sequence
  where dist n0 n1 =
          let x = min n0 n1
              y = max n0 n1
              Just d = HashMap.lookup (x, y) graph
          in d


part1 input =
  let (numbers, graph) = graphify input
  in ts numbers graph False
part2 input =
  let (numbers, graph) = graphify input
  in ts numbers graph True

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
