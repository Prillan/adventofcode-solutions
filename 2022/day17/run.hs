{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
import AoC
import AoC.Grid

import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

type N = Int

parseAll = filter (`elem` "<>")

-- bounds:
-- xmin = 1
-- xmax = 7
-- bottom = 0
-- start = max previous + 3

rocks =
  [ HashSet.fromList [(3,0),(4,0),(5,0),(6,0)] --  "-"
  , HashSet.fromList [(4,0),(3,1),(4,1),(5,1),(4,2)] -- "+"
  , HashSet.fromList [(3,0),(4,0),(5,0),(5,1),(5,2)]
  , HashSet.fromList [(3,0),(3,1),(3,2),(3,3)] -- "|"
  , HashSet.fromList [(3,0),(4,0),(3,1),(4,1)]
  ]

disjoint x y = HashSet.null $ x `HashSet.intersection` y

singleRock height rock locked jets = go (minimum $ map fst $ toList initial) (maximum $ map fst $ toList initial) initial jets
  where start = height + 4
        initial = HashSet.map (\(x, y) -> (x, y + start)) rock

        go xmin xmax r ((_, j):js) =
          let (xmin', xmax', r') = jetStep xmin xmax r j
              r'' = HashSet.map (\(x, y) -> (x, y - 1)) r'
          in
            if r'' `disjoint` locked
            then go xmin' xmax' r'' js
            else (r', js)

        jetStep xmin xmax r j =
          let (xmin', xmax', r') = case j of
                '>'
                  | xmax == 7 -> (xmin, xmax, r)
                  | otherwise -> (xmin + 1, xmax + 1, HashSet.map (\(x, y) -> (x + 1, y)) r)
                '<'
                  | xmin == 1 -> (xmin, xmax, r)
                  | otherwise -> (xmin - 1, xmax - 1, HashSet.map (\(x, y) -> (x - 1, y)) r)
          in
            if r' `disjoint` locked
            then (xmin', xmax', r')
            else (xmin, xmax, r)

chamberFloor :: HashSet (N, N)
chamberFloor = HashSet.fromList $ map (,0) [1..7]

type SetGrid = HashSet (N, N)

ppSetGrid :: (Bool -> Char) -> SetGrid -> String
ppSetGrid pp = ppGrid pp . fromSetGrid

fromSetGrid :: SetGrid -> [[Bool]]
fromSetGrid g =
  let (cs, rs) = unzip $ toList g
      cm = maximum cs
      rm = maximum rs
  in fromSetGrid' 7 (rm + 1) g

fromSetGrid' :: Int -> Int -> SetGrid -> [[Bool]]
fromSetGrid' w h g =
  map (map (`HashSet.member` g))
  $ [[(ci, ri) | ci <- [1..w]]  | ri <- [0..h]]

pp' = unlines . reverse . lines . ppSetGrid ppfun

ppfun = \case True -> '#'
              False -> '.'

fst3 (x, _, _) = x

part1 jets =
  fst3
  $ foldl' step (0, chamberFloor, cycle $ zip [0..] jets) (take 2022 $ cycle $ zip [0..] rocks)
  where step (height, locked, js) (_, rock) =
          let (r, js') = singleRock height rock locked js
              height' = maximum . map snd $ HashSet.toList r
          in (max height height', locked `HashSet.union` r, js')


part2 jets =
  map (\(s, h, g, _, c) -> (s, h, g, c))
  . drop 1
  $ scanl' step (Set.empty, 0, chamberFloor, cycle $ zip [0::Int ..] jets, (0,0,0,0,0,0)) (cycle $ zip [0::Int ..] rocks)
  where step (!seen, !height, !locked, js@((ji,_):_), _) (ri, rock) =
          let (r, js'@((ji', _):_)) = singleRock height rock locked js
              height' = max height (maximum . map snd $ HashSet.toList r)
              lowb = minimum $ map snd $ HashSet.toList r
              locked' = locked `HashSet.union` r
              ix = (ri, (ri + 1) `mod` 5, ji, ji', height, height')
          in case listToMaybe (topLine lowb height' locked') of
            Just h | h > 0 ->
                let locked'' = HashSet.filter ((>= 0) . snd)
                               .  HashSet.map (\(c, r) -> (c, 1 + r - h))
                               $ locked
                    seenix = tce "seen now" (ji, ri, locked'')
                in if seenix `Set.member` seen
                   then error "real wat"
                   else (Set.insert seenix seen, height' - h, locked'', js', ix)
            _ -> (seen, height', locked', js', ix)


topLine :: N -> N -> SetGrid -> [N]
topLine from to g =
  filter (\y -> all (\x -> (x, y) `HashSet.member` g) [1..7])
  [to,to-1..from]


main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
