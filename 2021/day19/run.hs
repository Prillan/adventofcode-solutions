{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bifunctor
import Data.Char
import Data.List.Split
import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

type Beacon = V3 N
data Scanner = Scanner { scannerId ::  Int
                       , scannerBeacons :: Set Beacon
                       }
  deriving Show

-- TODO: Cache "diffs", might be easier to match scanners if some
-- vectors are calculated.

shift :: V3 N -> Set Beacon -> Set Beacon
shift d = Set.map (+ d)

rotations =
  [ \case V3 (x, y, z) -> V3 ( x,  y,  z)
  , \case V3 (x, y, z) -> V3 ( x,  z, -y)
  , \case V3 (x, y, z) -> V3 ( x, -y, -z)
  , \case V3 (x, y, z) -> V3 ( x, -z,  y)
  , \case V3 (x, y, z) -> V3 ( y,  x, -z)
  , \case V3 (x, y, z) -> V3 ( y,  z,  x)
  , \case V3 (x, y, z) -> V3 ( y, -x,  z)
  , \case V3 (x, y, z) -> V3 ( y, -z, -x)
  , \case V3 (x, y, z) -> V3 ( z,  x,  y)
  , \case V3 (x, y, z) -> V3 ( z,  y, -x)
  , \case V3 (x, y, z) -> V3 ( z, -x, -y)
  , \case V3 (x, y, z) -> V3 ( z, -y,  x)
  , \case V3 (x, y, z) -> V3 (-x,  y, -z)
  , \case V3 (x, y, z) -> V3 (-x,  z,  y)
  , \case V3 (x, y, z) -> V3 (-x, -y,  z)
  , \case V3 (x, y, z) -> V3 (-x, -z, -y)
  , \case V3 (x, y, z) -> V3 (-y,  x,  z)
  , \case V3 (x, y, z) -> V3 (-y,  z, -x)
  , \case V3 (x, y, z) -> V3 (-y, -x, -z)
  , \case V3 (x, y, z) -> V3 (-y, -z,  x)
  , \case V3 (x, y, z) -> V3 (-z,  x, -y)
  , \case V3 (x, y, z) -> V3 (-z,  y,  x)
  , \case V3 (x, y, z) -> V3 (-z, -x,  y)
  , \case V3 (x, y, z) -> V3 (-z, -y, -x)
  ]

scannerRotations :: Set Beacon -> [Set Beacon]
scannerRotations s = map (\r -> Set.map r s) rotations

-- returns scanner pos relative to first scanner + rotated and shifted scanner
match :: Scanner -> Scanner -> Maybe (V3 N, Scanner)
match (Scanner _ s1) (Scanner s2id s2) = listToMaybe do
  -- Pick rotation
  -- Pick anchor beacon and scanner beacon
  -- Shift
  -- Check intersection
  -- If intersection >= 12 => locked
  rotated <- scannerRotations s2
  anchor  <- take 15 (Set.toList s1) -- 15 makes sure we get at least
                                     -- 1 from the overlap (26 - 12 =
                                     -- 14)
  anchor2 <- Set.toList rotated
  let d = anchor - anchor2
      shifted = shift d rotated
      overlap = s1 `Set.intersection` shifted
  if length overlap >= 12
    then pure (d, (Scanner s2id shifted))
    else []

-- TODO: Calculate the cache on-the-fly instead
cache :: [Scanner] -> Set (Int, Int)
cache scanners =
  let pairs = [ (s1, s2) | s1 <- scanners
                         , s2 <- scanners
                         , scannerId s1 < scannerId s2
                         ]
  in Set.fromList
     . map (bimap scannerId scannerId)
     . filter (isJust . uncurry match)
     $ pairs

cachedMatch :: Set (Int, Int) -> Scanner -> Scanner -> Maybe (V3 N, Scanner)
cachedMatch c base@(Scanner bid _) candidate@(Scanner cid _)
  | (min bid cid, max bid cid) `Set.member` c =
      match base candidate
  | otherwise = Nothing

merge :: [Scanner] -> [(V3 N, Scanner)]
merge (s:scanners) = go [(0, s)] scanners
  where c = cache (s:scanners)
        go locked = \case
          [] -> locked
          available ->
            let (merged, rem) = partitionWith (matchWith locked) available
            in
              go (locked ++ merged) rem

        matchWith locked candidate =
          let matched = mapMaybe (\(_, u) -> cachedMatch c u candidate) locked
          in case matched of
            x:_ -> Left x
            _   -> Right candidate


parse :: [String] -> Scanner
parse (header:beacons) =
  Scanner { scannerId = read . takeWhile isDigit . drop 12 $ header
          , scannerBeacons = Set.fromList (map f beacons)
          }
  where f x = v3 . read $ "(" ++ x ++ ")"

parseAll = map parse . splitOn [""] . lines

part1 :: [(V3 N, Scanner)] -> Int
part1 = length . Set.unions . map (scannerBeacons . snd)

part2 solved =
  let positions = map fst solved
  in maximum $ [ sum (abs (p1 - p2)) | p1 <- positions
                                     , p2 <- positions
                                     ]

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   let solved = merge input
   print (part1 solved)
   print (part2 solved)
