{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.List
import Data.List.Split (splitOn)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

parseAll =
  map (read @Int) . splitOn ","

numbers :: [Int] -> [Int]
numbers start =
  start ++ go (IntMap.fromList $ zip start [0..]) 0 (length start)
  where go :: IntMap Int -> Int -> Int -> [Int]
        go spoken speak t =
          let !next = case IntMap.lookup speak spoken of
                        Just turn -> (t - turn)
                        Nothing   -> 0
          in speak:go (IntMap.insert speak t spoken) next (t + 1)

part1 = (!! 2019) . numbers
part2 = (!! (30000000 - 1)) . numbers


-- Runs in ~75s
-- ./run  76,87s user 0,68s system 99% cpu 1:17,81 total
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
