{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type Pos = (Int, Int)

parseAll = readMapGrid @Int

neighbors :: Pos -> [Pos]
neighbors (i, j) = [ (i + dx, j + dy) | dx <- [-1..1]
                                      , dy <- [-1..1]
                                      , not (dx == 0 && dy == 0) ]

step :: MapGrid Int -> MapGrid Int
step = fixpoint flash . incAll
  where incAll = HashMap.map (+ 1)
        flash m =
          let toFlash = HashMap.keys $ HashMap.filter (> 9) m
              flashed = counter
                        . filter (not . flip elem toFlash)
                        . concatMap neighbors
                        $ toFlash
              f k v | v == 0                         = 0
                    | k `elem` toFlash               = 0
                    | Just x <- Map.lookup k flashed = v + x
                    | otherwise                      = v
          in HashMap.mapWithKey f m

part1 = length
        . filter (== 0)
        . concatMap HashMap.elems
        . take 101
        . iterate step

part2 =
  fst
  . head
  . filter (all (== 0) . HashMap.elems . snd)
  . zip [0..]
  . iterate step

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
