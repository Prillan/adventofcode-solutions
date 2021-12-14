{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Functor
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
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

-- e, se, sw, w, nw, and ne
data Dir = E | SE | SW | W | NW | NE
  deriving (Show, Read, Eq, Enum, Bounded)

dirs :: [Dir]
dirs = [minBound..maxBound]

type Tile = V2 Int

type Parser = Parsec Void String

dirP :: Parser Dir
dirP =
  choice [ E <$ string "e"
         , W <$ string "w"
         , SE <$ string "se"
         , SW <$ string "sw"
         , NW <$ string "nw"
         , NE <$ string "ne" ]

asVec :: Dir -> V2 Int
asVec = \case E  -> v2 ( 1,  0)
              W  -> v2 (-1,  0)
              SE -> v2 ( 0,  1)
              SW -> v2 (-1,  1)
              NE -> v2 ( 1, -1)
              NW -> v2 ( 0, -1)

vecDirs :: [V2 Int]
vecDirs = map asVec dirs

parseAll :: String -> [[Dir]]
parseAll =
  map (\(Right x) -> x)
  . map (parse (many dirP <* eof) "")
  . lines

setup :: [[Dir]] -> Set Tile
setup =
  Map.keysSet
  . Map.filter (\c -> (c `mod` 2) == 1)
  . counter
  . map (foldl' (+) 0 . map asVec)

aliveNeighbors :: Tile -> Set Tile -> Set Tile
aliveNeighbors t tiles =
  Set.intersection tiles (Set.fromList $ map (+ t) vecDirs)

rule :: Tile -> Set Tile -> Bool
rule t tiles =
  let nbs = aliveNeighbors t tiles
      alive = t `Set.member` tiles
  in case (Set.size nbs, alive) of
       (n,  True) | n == 0 -> False
                  | n  > 2 -> False
       (n, False) | n == 2 -> True
       _                   -> alive

step :: Set Tile -> Set Tile
step tiles =
  Set.filter (flip rule tiles)
  . Set.fromList
  . concatMap (\t -> t:map (+ t) vecDirs)
  $ Set.toList tiles

part1 :: [[Dir]] -> Int
part1 = Set.size . setup

part2 :: [[Dir]] -> Int
part2 input = Set.size (iterateN' 100 step (setup input))

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
