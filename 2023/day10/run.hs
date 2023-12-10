{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

-- TODO: Cleanup

import Control.Monad (guard)
import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord

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

type N = Int

data Dir = D | U | R | L
  deriving (Show, Eq, Enum, Bounded, Ord)

data Cell = Empty
          | Start
          | Pipe Dir Dir
  deriving (Show, Eq)

dirs = \case Pipe a b -> [a, b]
             _ -> []

mirror = \case D -> U
               U -> D
               L -> R
               R -> L

dirv :: Dir -> (N, N)
dirv = \case R -> ( 1,  0)
             L -> (-1,  0)
             U -> ( 0, -1)
             D -> ( 0,  1)

move (c, r) d =
  let (cd, rd) = dirv d
  in (c + cd, r + rd)

parseAll = parseMapGrid \case
  '-' -> Pipe R L
  'J' -> Pipe U L
  'L' -> Pipe U R
  '|' -> Pipe D U
  '7' -> Pipe D L
  'F' -> Pipe D R
  'S' -> Start
  '.' -> Empty

pcell = \case
  Pipe R L -> '-'
  Pipe U L -> 'J'
  Pipe U R -> 'L'
  Pipe D U -> '|'
  Pipe D L -> '7'
  Pipe D R -> 'F'
  Start -> 'S'
  Empty -> '.'
  Pipe a b -> pcell (Pipe b a)

follow mg n from =
  case (dirs $ mg HashMap.! n) \\ [mirror from] of
    [x] -> (move n x, x)

valid mg start = mapMaybe f [D, L, R, U]
  where f d = do
          let n = move start d
          next <- mg HashMap.!? n
          guard $ mirror d `elem` dirs next
          pure (n, d)

neighbors (ci, ri) =
  [ (ci + cd, ri + rd) | rd <- [-1, 0, 1]
                       , cd <- [-1, 0, 1]
                       , not (rd == 0 && cd == 0)
                       ]

loop mg = (start, mirror d):takeWhile (not . (== (start, mirror d))) (iterate (uncurry (follow mg)) v)
  where Just (start, _) = find ((== Start) . snd) $ HashMap.toList mg
        [v@(_, vd), (_, d)] = valid mg start

part1 mg = length (loop mg) `div` 2

simplify mg =
  let l@((start, d1):(_, d2):_) = loop mg
      lset = Set.fromList $ map fst l

      replace k = \case
        Empty -> Empty
        v | k `Set.member` lset -> v
        _ -> Empty

      simplified =
        HashMap.mapWithKey replace
        . HashMap.insert start (Pipe (mirror d1) d2)
        $ mg
  in
    simplified


degree simpl e = go 0 Nothing e
  where go !d !onoff = \case
          (_, -1) -> d
          (c, r) ->
            let n = (c, r - 1)
                !next = simpl HashMap.!? (c, r - 1)
                ds = sort $ maybe [] dirs next
                (!dd, !onoff') =
                  case (ds, onoff) of
                    -- D | U | R | L
                    ([R, L], Nothing) -> (1, Nothing)
                    ([D, R], Just R)  -> (0, Nothing)
                    ([D, R], Just L)  -> (1, Nothing)

                    ([D, L], Just L)  -> (0, Nothing)
                    ([D, L], Just R)  -> (1, Nothing)

                    ([U, L], Nothing) -> (0, Just L)
                    ([U, R], Nothing) -> (0, Just R)

                    ([D, U], Just _)  -> (0, onoff)
                    ([], _)           -> (0, onoff)
                    v -> error $ "impossible: " <> show v
            in
              go (d + dd) onoff' n

part2 mg =
  let simpl = simplify mg
      empties = HashMap.keys $ HashMap.filter (== Empty) simpl
  in
    length $ filter odd $ map (degree simpl) empties

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
