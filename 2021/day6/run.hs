{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

parseAll input = read @[Int] $ "[" ++ input ++ "]"

lantern = iterate go
  where go = HashMap.fromListWith (+) . concatMap f . HashMap.toList
        f (0, !n) = [(6, n), (8, n)]
        f (i, !n) = [(i - 1, n)]


simulate = lantern . HashMap.fromListWith (+) . map (,1)

part1 = sum . (!! 80) . simulate
part2 = sum . (!! 256) . simulate

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
