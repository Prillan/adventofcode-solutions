{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid
import AoC.Search (dijkstra_)

import Data.List
import Data.Maybe

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

parseAll = readMapGrid @Int

type Pos = (Int, Int)

hv :: Pos -> [Pos]
hv (i, j) = [ (i + dx, j + dy) | dx <- [-1..1]
                               , dy <- [-1..1]
                               , dx == 0 || dy == 0 ]



hvNeighbors :: MapGrid Int -> Pos -> [(Pos, Int)]
hvNeighbors m =
  mapMaybe (\pos -> sequence (pos, pos `HashMap.lookup` m))
  . hv

minPathCost :: MapGrid Int -> Int
minPathCost grid =
  let br = maximum $ HashMap.keys grid
      Just cost = dijkstra_ (br ==)
                            (hvNeighbors grid)
                            (0, 0)
  in cost

extend :: Int -> [Int] -> [Int]
extend = go
  where go 0 xs = xs
        go n xs = xs ++ map (\case i | i == 9    -> 1
                                     | otherwise -> i + 1) (go (n - 1) xs)

part1 = minPathCost . toMapGrid
part2 =
  minPathCost
  . toMapGrid
  . transpose
  . map (extend 4)
  . transpose
  . map (extend 4)

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- readGrid @Int <$> readFile file
   print (part1 input)
   print (part2 input)
