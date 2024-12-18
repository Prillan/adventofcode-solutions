{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC.Grid

import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.Maybe

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

type Pos = (Int, Int)
type Dir = (Int, Int)
type Input = (MapGrid Char, Pos, Pos)

parseAll :: String -> Input
parseAll input =
  let g = parseMapGrid id input
      Just (start, _) = find (\(_, v) -> v == 'S') $ HashMap.toList g
      Just (  end, _) = find (\(_, v) -> v == 'E') $ HashMap.toList g
  in ( HashMap.insert start '.' $ HashMap.insert end '.' g
     , start
     , end
     )

rotateCW, rotateCCW :: Dir -> Dir
rotateCW  (x, y) = (-y,  x)
rotateCCW (x, y) = ( y, -x)

neighbors :: MapGrid Char -> Pos -> Dir -> [((Pos, Dir), N)]
neighbors g p@(x, y) d@(dx, dy) =
  let ps = maybeToList do
        let q = (x + dx, y + dy)
        '.' <- g HashMap.!? q
        pure ((q, d), 1)
      rs = do
        r <- [ rotateCW  d
             , rotateCCW d
             ]
        pure ((p, r), 1000)
  in ps ++ rs

parts :: Input -> [Int]
parts (g, s, e) =
  let stop = (== e) . fst
      next = uncurry (neighbors g)
      Just (cost, end, cf) = dijkstra'' stop next (s, (1, 0))
  in
    [ cost
    , (1+) . length . Set.fromList . concatMap (map fst) $ traversePath cf end
    ]

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   mapM_ print (parts input)

-- TODO: Move to AoC.Search

traversePath :: Hashable node => HashMap node (cost, [node]) -> node -> [[node]]
traversePath cf end = go end
  where go curr =
          case cf HashMap.!? curr of
            Just (_, []) -> [[]]
            Just (_, nodes) ->
              concatMap (\n -> map (n:) (go n)) nodes
            Nothing -> []

removeIfWorse :: Ord cost => (cost, [node]) -> (cost, [node]) -> Maybe (cost, [node])
removeIfWorse n@(new, _) (old, _)
  | new <= old = Just n
  | otherwise  = Nothing
{-# INLINE removeIfWorse #-}

dijkstra'' :: (Hashable node, Ord cost, Num cost, Ord node)
        => (node -> Bool)
        -> (node -> [(node, cost)])
        -> node
        -> Maybe (cost, node, HashMap node (cost, [node]))
dijkstra'' stop next start = go (HM.singleton start (0, [])) (PQueue.singleton 0 start)
  where --combine :: Ord cost => (cost, [node]) -> (cost, [node]) -> (cost, [node])
        combine (c1, n1) (c2, n2) | c1 < c2   = (c1, n1)
                                  | c1 > c2   = (c2, n2)
                                  | otherwise = (c1, nub $ n1 ++ n2)
        go visited q =
          case PQueue.minViewWithKey q of
            Just ((!cost, !current), rest)
              | stop current -> Just (cost, current, visited)
              | otherwise ->
                let nexts = HM.fromList $ map (second (\c -> (c + cost, [current]))) (next current)
                    nexts' = HM.differenceWith removeIfWorse nexts visited
                    toCheck =
                      PQueue.fromList
                      . map (\(x, (c, _)) -> (c, x))
                      . HM.toList
                      $ nexts'
                in
                  go --(HM.union (HM.map (const current) nexts') cameFroms)
                     (HM.unionWith combine visited nexts')
                     (toCheck `PQueue.union` rest)
            _ -> Nothing
