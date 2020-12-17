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

type G = Set (Int, Int, Int)

parse '#' = True
parse _ = False

parseAll :: String -> G
parseAll =
  Set.fromList
  . map (\(x, y) -> (x, y, 0))
  . map fst
  . filter snd
  . Map.toList
  . parseMapGrid parse

dirs :: [(Int, Int, Int)]
dirs =
  drop 1 $ (,,) <$> [0, -1, 1] <*> [0, -1, 1] <*> [0, -1, 1]

neighbors g (x, y, z) =
  filter (flip Set.member g)
  . map (\(dx, dy, dz) -> (x + dx, y + dy, z + dz))
  $ dirs

step :: (Int, Int, Int) -> G -> ((Int, Int, Int), G)
step (ix, iy, iz) g =
  let g' =
        Set.fromList
        . filter (rule g)
        $ (,,) <$> [-ix..ix] <*> [-iy..iy] <*> [-iz..iz]
  in ((ix + 1, iy + 1, iz + 1), g')

rule :: G -> (Int, Int, Int) -> Bool
rule g pos =
  case (Set.member pos g, length . neighbors g $ pos) of
    (True, alive)
      | alive == 2 || alive == 3 -> True
      | otherwise -> False
    (_, 3) -> True
    _ -> False

part1 initial =
  let (_, afterBoot) = iterate (uncurry step) ((8,8,1), initial) !! 6
  in Set.size afterBoot


-- PART 2

type G4 = Set (Int, Int, Int, Int)

parseAll4 :: String -> G4
parseAll4 =
  Set.fromList
  . map (\(x, y) -> (x, y, 0, 0))
  . map fst
  . filter snd
  . Map.toList
  . parseMapGrid parse

dirs4 :: [(Int, Int, Int, Int)]
dirs4 =
  drop 1 $ (,,,) <$> [0, -1, 1] <*> [0, -1, 1] <*> [0, -1, 1] <*> [0, -1, 1]

neighbors4 g (x, y, z, w) =
  filter (flip Set.member g)
  . map (\(dx, dy, dz, dw) -> (x + dx, y + dy, z + dz, w + dw))
  $ dirs4

step4 :: (Int, Int, Int, Int) -> G4 -> ((Int, Int, Int, Int), G4)
step4 (ix, iy, iz, iw) g =
  let g' =
        Set.fromList
        . filter (rule4 g)
        $ (,,,) <$> [-ix..ix] <*> [-iy..iy] <*> [-iz..iz] <*> [-iw..iw]
  in ((ix + 1, iy + 1, iz + 1, iw + 1), g')

rule4 :: G4 -> (Int, Int, Int, Int) -> Bool
rule4 g pos =
  case (Set.member pos g, length . neighbors4 g $ pos) of
    (True, alive)
      | alive == 2 || alive == 3 -> True
      | otherwise -> False
    (_, 3) -> True
    _ -> False


part2 initial =
  let (_, afterBoot) = iterate (uncurry step4) ((8,8,1,1), initial) !! 6
  in Set.size afterBoot

-- Runs in ~2s
-- ./run  2,18s user 0,00s system 99% cpu 2,193 total
main = do
  input <- readFile "input.txt"
  print (part1 $ parseAll input)
  print (part2 $ parseAll4 input)
