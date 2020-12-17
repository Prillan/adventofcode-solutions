{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- type Parser = Parsec (ErrorItem Char) String

type G = Map (Int, Int, Int) Bool

parse '#' = True
parse _ = False

parseAll :: String -> G
parseAll =
  Map.mapKeys (\(x, y) -> (x, y, 0))
  . parseMapGrid parse

dirs :: [(Int, Int, Int)]
dirs =
  drop 1 $ (,,) <$> [0, -1, 1] <*> [0, -1, 1] <*> [0, -1, 1]

neighbors g (x, y, z) =
  mapMaybe (flip Map.lookup g)
  . map (\(dx, dy, dz) -> (x + dx, y + dy, z + dz))
  $ dirs

step :: (Int, Int, Int) -> G -> ((Int, Int, Int), G)
step (ix, iy, iz) g =
  let g' =
        Map.fromList
        . map (\pos -> (pos, rule g pos))
        $ (,,) <$> [-ix..ix] <*> [-iy..iy] <*> [-iz..iz]
  in ((ix + 1, iy + 1, iz + 1), g')

rule :: G -> (Int, Int, Int) -> Bool
rule g pos =
  case (Map.lookup pos g, length . filter id . neighbors g $ pos) of
    (Just True, alive)
      | alive == 2 || alive == 3 -> True
      | otherwise -> False
    (_, 3) -> True
    _ -> False

part1 initial =
  let (_, afterBoot) = iterate (uncurry step) ((8,8,1), initial) !! 6
  in length . filter id $ Map.elems afterBoot


-- PART 2

type G4 = Map (Int, Int, Int, Int) Bool

parseAll4 :: String -> G4
parseAll4 =
  Map.mapKeys (\(x, y) -> (x, y, 0, 0))
  . parseMapGrid parse

dirs4 :: [(Int, Int, Int, Int)]
dirs4 =
  drop 1 $ (,,,) <$> [0, -1, 1] <*> [0, -1, 1] <*> [0, -1, 1] <*> [0, -1, 1]

neighbors4 g (x, y, z, w) =
  mapMaybe (flip Map.lookup g)
  . map (\(dx, dy, dz, dw) -> (x + dx, y + dy, z + dz, w + dw))
  $ dirs4

step4 :: (Int, Int, Int, Int) -> G4 -> ((Int, Int, Int, Int), G4)
step4 (ix, iy, iz, iw) g =
  let g' =
        Map.fromList
        . map (\pos -> (pos, rule4 g pos))
        $ (,,,) <$> [-ix..ix] <*> [-iy..iy] <*> [-iz..iz] <*> [-iw..iw]
  in ((ix + 1, iy + 1, iz + 1, iw + 1), g')

rule4 :: G4 -> (Int, Int, Int, Int) -> Bool
rule4 g pos =
  case (Map.lookup pos g, length . filter id . neighbors4 g $ pos) of
    (Just True, alive)
      | alive == 2 || alive == 3 -> True
      | otherwise -> False
    (_, 3) -> True
    _ -> False


part2 initial =
  let (_, afterBoot) = iterate (uncurry step4) ((8,8,1,1), initial) !! 6
  in length . filter id $ Map.elems afterBoot

-- Runs in ~6s
-- ./run  6,45s user 0,04s system 99% cpu 6,498 total
main = do
  input <- readFile "input.txt"
  print (part1 $ parseAll input)
  print (part2 $ parseAll4 input)
