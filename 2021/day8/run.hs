{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
import AoC
import AoC.Grid

import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List.Split
import Data.Foldable (toList)

-- TODO: Improve, this is silly.

parse line =
  let [a, b] = splitOn ["|"] . map sort $ words line
  in (a, b)

parseAll = map parse . lines

lkp = \case "abcefg"  -> Just 0
            "cf"      -> Just 1
            "acdeg"   -> Just 2
            "acdfg"   -> Just 3
            "bcdf"    -> Just 4
            "abdfg"   -> Just 5
            "abdefg"  -> Just 6
            "acf"     -> Just 7
            "abcdefg" -> Just 8
            "abcdfg"  -> Just 9
            _         -> Nothing

rev = \case 0 -> "abcefg"
            1 -> "cf"
            2 -> "acdeg"
            3 -> "acdfg"
            4 -> "bcdf"
            5 -> "abdfg"
            6 -> "abdefg"
            7 -> "acf"
            8 -> "abcdefg"
            9 -> "abcdfg"

llkp :: HashMap Char Char -> String -> Maybe Int
llkp m = lkp . sort . map (m HashMap.!)

llkpNum :: HashMap Char Char -> [String] -> Maybe Int
llkpNum m = fmap (read @Int . concatMap show) . traverse (llkp m)

initial :: [String] -> HashMap Char [Char]
initial xs =
  let Just d1 = find ((== 2) . length) xs
      Just d4 = find ((== 4) . length) xs
      Just d7 = find ((== 3) . length) xs
      Just d8 = find ((== 7) . length) xs

  in HashMap.fromListWith intersect $ map (,rev 1) d1
                               ++ map (,rev 4) d4
                               ++ map (,rev 7) d7
                               ++ map (,rev 8) d8

solve xs = filter (\m -> all (isJust . llkp m) xs)
           . map (HashMap.fromList . zip "abcdefg")
           . filter ((== 7) . length . nub)
           . sequence
           . toList
           $ initial xs

type Entry = ([String], [String])

part1 :: [Entry] -> Int
part1 = length . concatMap (filter simple) . map snd
  where simple x = let l = length x in l `elem` [2, 4, 3, 7] -- [1, 4, 7, 8]


part2 :: [Entry] -> Int
part2 = sum . map f
  where f (input, output) =
          let [m] = solve input
              Just n = llkpNum m output
          in n

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
