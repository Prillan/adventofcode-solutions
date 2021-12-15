module AoC.Search (dijkstra_) where

import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue


dijkstra_ :: (Hashable node, Ord cost, Num cost, Eq node)
          => (node -> Bool)
          -> (node -> [(node, cost)])
          -> node
          -> cost
dijkstra_ stop next start = go (HashSet.singleton start) (PQueue.singleton 0 start)
  where go visited q =
          case PQueue.minViewWithKey q of
            Just ((!cost, !current), rest)
              | stop current -> cost
              | otherwise ->
                let nexts = filter (\(x, _) -> not (x `HashSet.member` visited))
                            $ next current
                    nextCandidates = 
                      PQueue.fromList
                      . map (\(x, c) -> (cost + c, x))
                      $ nexts
                in
                  go (HashSet.union visited (HashSet.fromList $ map fst nexts))
                     (PQueue.union nextCandidates rest)
