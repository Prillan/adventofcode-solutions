{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC hiding (tce)
import qualified AoC
import AoC.Grid

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
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Control.Monad (guard)

type N = Int

type SetGrid = HashSet (N, N)
type Bounds = ((N, N), (N, N))

getBounds g =
  let (rs, cs) = unzip $ HashSet.toList g
  in ((minimum rs, minimum cs), (maximum rs, maximum cs))

parse = id

parseAll :: String -> (SetGrid, Bounds)
parseAll input =
  let s = HashMap.keysSet
        . HashMap.filter (== '#')
        . parseMapGrid id
        $ input
  in (s, getBounds s)

data Dir = N | S | W | E
  deriving (Show, Eq, Ord, Enum, Bounded)

next :: (Eq a, Enum a, Bounded a) => a -> a
next d | d == maxBound = minBound
       | otherwise     = succ d

setup :: SetGrid -> HashSet (V2 N)
setup = HashSet.map v2

neighbors g pos =
  \case N -> filter (not . (`HashMap.member` g)) [ pos + v2 (-1, -1)
                                                 , pos + v2 ( 0, -1)
                                                 , pos + v2 ( 1, -1)
                                                 ]
        S -> filter (not . (`HashMap.member` g)) [ pos + v2 (-1, 1)
                                                 , pos + v2 ( 0, 1)
                                                 , pos + v2 ( 1, 1)
                                                 ]
        W -> filter (not . (`HashMap.member` g)) [ pos + v2 (-1,  1)
                                                 , pos + v2 (-1,  0)
                                                 , pos + v2 (-1, -1)
                                                 ]
        E -> filter (not . (`HashMap.member` g)) [ pos + v2 (1,  1)
                                                 , pos + v2 (1,  0)
                                                 , pos + v2 (1, -1)
                                                 ]

dirv =
  \case N -> v2 ( 0, -1)
        S -> v2 ( 0,  1)
        W -> v2 (-1,  0)
        E -> v2 ( 1,  0)

dirNeighbors =
  \case N -> [ v2 (-1, -1)
             , v2 ( 0, -1)
             , v2 ( 1, -1)
             ]
        S -> [ v2 (-1, 1)
             , v2 ( 0, 1)
             , v2 ( 1, 1)
             ]
        W -> [ v2 (-1,  1)
             , v2 (-1,  0)
             , v2 (-1, -1)
             ]
        E -> [ v2 (1,  1)
             , v2 (1,  0)
             , v2 (1, -1)
             ]


freeNeighbors g pos =
  filter (not . (`HashSet.member` g))
  . map ((+ pos) . v2)
  $ [ ( 0,  1)
    , ( 0, -1)
    , ( 1,  0)
    , ( 1,  1)
    , ( 1, -1)
    , (-1,  0)
    , (-1,  1)
    , (-1, -1)
    ]
elfPropose :: HashSet (V2 N) -> V2 N -> Dir -> Maybe (Dir, V2 N)
elfPropose g pos dir = do
  let !free = tce "free" $ freeNeighbors g pos
  guard $ length free  /= 8
  let options = take 4 $ iterate next dir
  proposed <- find (\d -> all (`elem` free) (map (+ pos) (dirNeighbors d))) options
  pure (proposed, pos + dirv proposed)

propose g dir = go HashSet.empty HashMap.empty HashSet.empty (HashSet.toList g)
  where go !bad !seen !new =
          \case [] -> new
                !pos:rest ->
                  case tce "proposed" $ elfPropose g (tce "pos" pos) (tce "dir" dir) of
                    Just (_, !pos')
                      | pos' `HashSet.member` (tce "bad" bad) ->
                          go bad
                             seen
                             (HashSet.insert (tce "bad pos" pos) new)
                             rest
                      | Just conflict <- tce "conflict" $ (tce "seen" seen) HashMap.!? pos' ->
                          go (HashSet.insert pos' bad)
                             (HashMap.delete pos' seen)
                             (HashSet.insert pos
                               . HashSet.insert (tce "conflict" conflict)
                               . HashSet.delete pos'
                               $ new)
                             rest
                      | otherwise ->
                          go bad
                             (HashMap.insert pos' pos seen)
                             (HashSet.insert pos' (tce "new" new))
                             rest
                    Nothing -> go bad seen (HashSet.insert pos (tce "new" new)) rest

step (g, dir) =
  (propose g dir, next dir)

empty g =
  let ((xmin, ymin), (xmax, ymax)) = getBounds $ HashSet.map (\(V2 t) -> t) g
  in
    (1 + xmax - xmin) * (1 + ymax - ymin) - fromIntegral (length g)

tce :: Show a => String -> a -> a
--tce = AoC.tce
tce _ = id
{-# INLINE tce #-}

part1 (g, b) =
  let (end, _) = iterate step (setup g, N) !! 10
  in
    (end, empty end)
part2 (g, _) =
  fixpointi step (setup g, N)

unv2 (V2 (x, y)) = (x, y)

pp g =
  let ((xmin, ymin), (xmax, ymax)) = getBounds
                                     . HashSet.map unv2
                                     $ g
      listGrid = [ [ v2 (x, y) `HashSet.member` g
                   | x <- [xmin..xmax]
                   ]
                 | y <- [ymin..ymax]
                 ]
  in
    putStrLn $ ppGrid (\case True -> '#'
                             False -> '.') listGrid

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

smallMain :: IO ()
smallMain = main' "small.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
