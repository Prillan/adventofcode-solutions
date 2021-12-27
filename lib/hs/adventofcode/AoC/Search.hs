module AoC.Search ( astar_
                  , bfs_
                  , dijkstra
                  , dijkstra_ ) where

import Data.Bifunctor (second)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue

-- Note: bfs_, dijkstra_ and astar_ all return only the cost of the
-- shortest path. If necessary, corresponding versions that returns
-- the path will be added in the future.

-- TODO: bfs_ with additional stop (fail) conditions? See 2016, day13.

bfs_ :: (Hashable node, Ord cost, Num cost, Eq node)
     => (node -> Bool)
     -> (node -> [node])
     -> node
     -> Maybe cost
bfs_ stop next = dijkstra_ stop (map (,1) . next)
{-# INLINABLE bfs_ #-}

removeIfWorse :: Ord a => a -> a -> Maybe a
removeIfWorse new old
  | new < old = Just new
  | otherwise = Nothing
{-# INLINE removeIfWorse #-}

-- Note: The body of dijkstra_ and astar_ are very similar, the reason
-- why dijkstra_ isn't defined in terms of astar_ is because of the
-- PQueue. For dijkstra_ we can get the cost directly from the queue
-- key, in astar_ we have to insert it in the queue value.
dijkstra :: (Hashable node, Ord cost, Num cost, Eq node)
         => (node -> Bool)
         -> (node -> [(node, cost)])
         -> node
         -> Maybe (node, cost)
dijkstra stop next start = go (HM.singleton start 0) (PQueue.singleton 0 start)
  where go visited q =
          case PQueue.minViewWithKey q of
            Just ((!cost, !current), rest)
              | stop current -> Just (current, cost)
              | otherwise ->
                let nexts = HM.fromList $ map (second (+ cost)) (next current)
                    nexts' = HM.differenceWith removeIfWorse nexts visited
                    toCheck =
                      PQueue.fromList
                      . map (\(x, c) -> (c, x))
                      . HM.toList
                      $ nexts'
                in
                  go (HM.unionWith min visited nexts')
                     (toCheck `PQueue.union` rest)
            _ -> Nothing
{-# INLINABLE dijkstra #-}

dijkstra_ :: (Hashable node, Ord cost, Num cost, Eq node)
          => (node -> Bool)
          -> (node -> [(node, cost)])
          -> node
          -> Maybe cost
dijkstra_ stop next start = snd <$> dijkstra stop next start
{-# INLINABLE dijkstra_ #-}

-- TODO: Fix astar
astar_ :: (Hashable node, Ord cost, Num cost, Eq node)
       => (node -> Bool)
       -> (node -> [(node, cost)])
       -> (node -> cost)
       -> node
       -> Maybe cost
astar_ stop next h start = go (HS.singleton start) (PQueue.singleton 0 (0, start))
  where go visited q =
          case PQueue.minView q of
            Just ((!cost, !current), rest)
              | stop current -> Just cost
              | otherwise ->
                let nexts = HM.fromList (next current) `HM.difference` HS.toMap visited
                    toCheck =
                      PQueue.fromList
                      . map (\(x, c) -> (h x + cost + c, (cost + c, x)))
                      . HM.toList
                      $ nexts
                in
                  go (visited `HS.union` HM.keysSet nexts)
                     (toCheck `PQueue.union` rest)
            _ -> Nothing
{-# INLINABLE astar_ #-}
