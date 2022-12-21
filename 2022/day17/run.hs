{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
import AoC.Grid

import Data.Foldable
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)

type N = Int

parseAll :: String -> String
parseAll = filter (`elem` "<>")

-- bounds:
-- xmin = 1
-- xmax = 7
-- bottom = 0
-- start = max previous + 3

rocks :: [SetGrid]
rocks =
  [ HashSet.fromList [(3,0),(4,0),(5,0),(6,0)] --  "-"
  , HashSet.fromList [(4,0),(3,1),(4,1),(5,1),(4,2)] -- "+"
  , HashSet.fromList [(3,0),(4,0),(5,0),(5,1),(5,2)]
  , HashSet.fromList [(3,0),(3,1),(3,2),(3,3)] -- "|"
  , HashSet.fromList [(3,0),(4,0),(3,1),(4,1)]
  ]

disjoint :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
disjoint x y = HashSet.null $ x `HashSet.intersection` y

singleRock :: N -> SetGrid -> SetGrid -> [(N, Char)] -> (SetGrid, [(N, Char)])
singleRock height rock locked =
  go (minimum $ map fst $ toList initial) (maximum $ map fst $ toList initial) initial
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
  let h = maximum . map snd $ toList g
  in fromSetGrid' 7 (h + 1) g

fromSetGrid' :: Int -> Int -> SetGrid -> [[Bool]]
fromSetGrid' w h g =
  map (map (`HashSet.member` g))
  $ [[(ci, ri) | ci <- [1..w]]  | ri <- [0..h]]

pp' :: SetGrid -> String
pp' = unlines . reverse . lines . ppSetGrid ppfun

ppfun :: Bool -> Char
ppfun = \case True -> '#'
              False -> '.'

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

simulate :: Int -> String -> (N, SetGrid, [(N, Char)])
simulate steps jets = simulate' rocks jets steps chamberFloor

simulate' :: [SetGrid] -> String -> Int -> SetGrid -> (N, SetGrid, [(N, Char)])
simulate' rocks jets steps starting =
  foldl' step (maximum . map snd $ HashSet.toList starting
              , starting
              , cycle $ zip [0..] jets) (take steps $ cycle rocks)
  where step (height, locked, js) rock =
          let (r, js') = singleRock height rock locked js
              height' = maximum . map snd $ HashSet.toList r
          in (max height height', locked `HashSet.union` r, js')


part1 :: String -> N
part1 = fst3 . simulate 2022

findCycle :: String -> (N, N, N, N, (Int, Int, HashSet (N, N)))
findCycle jets = go 0 Map.empty 0 0 chamberFloor (cycle $ zip [0::Int ..] jets) (cycle $ zip [0::Int ..] rocks)
  where go n !seen !height !floorPos !locked js@((ji,_):_) ((ri, rock):rs) =
          let (r, js') = singleRock height rock locked js
              height' = max height (maximum . map snd $ HashSet.toList r)
              lowb = minimum $ map snd $ HashSet.toList r
              locked' = locked `HashSet.union` r
          in case listToMaybe (topLine lowb height' locked') of
            Just h | h > 0 ->
                let locked'' = HashSet.filter ((>= 0) . snd)
                               .  HashSet.map (\(c, r) -> (c, r - h))
                               $ locked'
                    seenix = (ji, ri, locked'')
                    floorPos' = floorPos + h
                in case Map.lookup seenix seen of
                     Just (n', oldFloor) -> (oldFloor, floorPos', n', n+1, seenix)
                     Nothing ->
                       go (n+1) (Map.insert seenix (n+1, floorPos') seen) (height' - h) floorPos' locked''  js' rs
            _ -> go (n+1) seen height' floorPos locked' js' rs

topLine :: N -> N -> SetGrid -> [N]
topLine from to g =
  filter (\y -> all (\x -> (x, y) `HashSet.member` g) [1..7])
  [to,to-1..from]

simulateQuick :: Integer -> String -> (Integer, SetGrid)
simulateQuick steps jets =
  let (floor1, floor2, n1, n2, (ji, ri, g)) = findCycle jets
      cycleLength = fromIntegral $ n2 - n1
      cycleFloorDiff = fromIntegral $ floor2 - floor1
      target = steps - fromIntegral n1
      (k, r) = target `divMod` cycleLength

      finalFloor = k * cycleFloorDiff + fromIntegral floor1
      (finalH, finalG, _) = simulate' (drop ri $ cycle rocks) (drop ji $ cycle jets) (fromInteger r) g

  in
    (finalFloor + fromIntegral finalH, finalG)

part2 :: String -> Integer
part2 = fst . simulateQuick 1000000000000

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
