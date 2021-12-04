{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- TODO: Be more clever, this is naive, but it works.

type Board = [[Int]]

parse input =
  let (nums:rest) = lines input
  in (parseNums nums, parseBoards rest)

parseNums nums = read @[Int] $ "[" ++ nums ++ "]"

parseBoards :: [String] -> [Board]
parseBoards =
  chunksOf 5
  . map (map read . words)
  . filter (not . null)

candidates :: Board -> [[Int]]
candidates b = b ++ transpose b

-- weird, and inefficient. candidates produces all rows and columns,
-- then we repeatedly remove the played numbers from each candidate
-- and check if any are empty (meaning all were marked).
--
-- keep the numbers played and the board for later use...
play :: [Int] -> Board -> [(Bool, [Int], Board)]
play nums b =
  map (\ns -> (any (null . (\\ ns)) cs, ns, b))
  $ inits nums
  where cs = candidates b


fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

part1 :: ([Int], [Board]) -> Int
part1 (nums, bs) =
  let (_, ns, b) = head      -- pick it
        . filter fst3        -- find the entry that won
        . head               -- pick it
        . filter (any fst3)  -- find the step with at least one marked row/column
        . transpose          -- "zip" each sequence
        $ map (play nums) bs -- play each board
  in
    sum (concat b \\ ns) * last ns -- score

part2 :: ([Int], [Board]) -> Int
part2 (nums, bs) =
  let (_, _, b) = -- same as part1, except we pick the step with a
                  -- single non-winning board, and extract it
        head
        . filter (not . fst3)
        . head
        . filter ((== 1) . length . filter (not . fst3))
        . transpose
        $ map (play nums) bs

      (_, ns, _) =
        head
        . filter fst3
        $ play nums b -- play it until we win
  in
    sum (concat b \\ ns) * last ns

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parse <$> readFile file
   print (part1 input)
   print (part2 input)
