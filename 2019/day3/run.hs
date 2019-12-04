module Main (main) where

import Data.Char (isDigit)
import Data.Ord (comparing)
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP


data Instr = Instr Direction Integer
  deriving (Show)

instance Read Instr where
  readsPrec _ = readP_to_S $ do
    dir <- pure <$> get
    num <- many1 (satisfy isDigit)

    pure $ Instr (read dir) (read num)

data Direction = R | L | U | D
  deriving (Show, Read, Eq)

parseAll :: String -> [[Instr]]
parseAll =
  map read . map (\x -> "[" ++ x ++ "]") . lines

draw :: [Instr] -> [(Integer, (Integer, Integer))]
draw = draw' 0 (0, 0)

draw' :: Integer
  -> (Integer, Integer)
  -> [Instr]
  -> [(Integer, (Integer, Integer))]
draw' _ _ [] = []
draw' steps (cx, cy) (Instr dir dist:rest) =
  let (dx, dy) =
        case dir of
          R -> (1, 0)
          L -> (-1, 0)
          U -> (0, 1)
          D -> (0, -1)
      line = [(steps + i, (cx + i * dx, cy + i * dy)) | i <- [1..dist]]
  in
    line ++ draw' (steps + dist) (cx + dist * dx, cy + dist * dy) rest

part1 :: [Instr] -> [Instr] -> Integer
part1 firstWire secondWire =
  let firstPts = Set.fromList $ map snd $ draw firstWire
      secondPts = Set.fromList $ map snd $ draw secondWire
  in
    minimum
    $ map (\(x, y) -> abs x + abs y)
    $ Set.toList
    $ firstPts `Set.intersection` secondPts

part2 :: [Instr] -> [Instr] -> Integer
part2 firstWire secondWire =
  let mapify = Map.fromList
        . map (\(steps, pt) -> (pt, steps))
        . sortBy (comparing (negate . fst))
      firstPts = mapify $ draw firstWire
      secondPts = mapify $ draw secondWire

      common = Map.keysSet firstPts `Set.intersection` Map.keysSet secondPts
  in
    minimum
    $ map (\pt -> firstPts Map.! pt + secondPts Map.! pt)
    $ Set.toList common

main :: IO ()
main = do
  [firstWire, secondWire] <- parseAll <$> readFile "input.txt"
  print (part1 firstWire secondWire)
  print (part2 firstWire secondWire)
