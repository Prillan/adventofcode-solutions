{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC.Grid

import Control.Monad (guard)
import Data.Foldable (find)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

type Pos = (Int, Int)
type Input = (HashSet Pos, Pos, Pos)

parseAll :: String -> Input
parseAll input =
  let g = parseMapGrid id input
      Just (start, _) = find ((== 'S') . snd) $ HashMap.toList g
      Just (end, _)   = find ((== 'E') . snd) $ HashMap.toList g
  in
    ( HashMap.keysSet $ HashMap.filter (== '#') g
    , start
    , end)

(|+|) :: Pos -> Pos -> Pos
(x, y) |+| (dx, dy) = (x + dx, y + dy)

dirs :: [Pos]
dirs = [ ( 1,  0)
       , (-1,  0)
       , ( 0,  1)
       , ( 0, -1)
       ]

distances :: Hashable node => (node -> [node]) -> node -> HashMap node Int
distances next start = go (HashMap.singleton start 0) [start]
  where go visited =
          \case []   -> visited
                n:ns ->
                  let nbhd = next n
                      c = visited HashMap.! n
                      nexts = HashMap.fromList $ map (, c + 1) nbhd
                      visited' = HashMap.unionWith min visited nexts
                      toVisit = HashMap.difference nexts visited
                  in go visited' (HashMap.keys toVisit <> ns)

neighbors :: HashSet Pos -> Pos -> [Pos]
neighbors g n =
  filter (not . (`HashSet.member` g))
  $ map (n |+|) dirs

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

findCheats :: HashMap Pos Int
           -> HashMap Pos Int
           -> Int
           -> Int
           -> [(Pos, Pos)]
findCheats fromStart fromEnd target maxTime = pairs
  where pairs = do
          (cstart, cstartCost) <- HashMap.toList fromStart
          guard $ cstartCost < target
          (cend, cendCost) <- HashMap.toList fromEnd
          let d = dist cstart cend
              cost = cstartCost + cendCost + d
          guard $ 0 < d
            && d <= maxTime
            && cost <= target
          pure (cstart, cend)

parts :: Int -> Input -> [Int]
parts limit (g, s, e) =
  let !fromStart = distances (neighbors g) s
      !fromEnd   = distances (neighbors g) e
      l = fromStart HashMap.! e
      target = l - limit
  in map (length . findCheats fromStart fromEnd target) [2, 20]

main :: IO ()
main = main' 100 "input.txt"

exampleMain :: IO ()
exampleMain = main' 64 "example.txt"

main' :: Int -> FilePath -> IO ()
main' limit file = do
   input <- parseAll <$> readFile file
   mapM_ print (parts limit input)
