{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid.New as Grid

import Control.Monad (guard)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)

import qualified Data.Array.IArray as A
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

type Input = (UArrayGrid Char, (N, N))

parseAll :: String -> Input
parseAll input =
  let g = Grid.parse id input
  in
    ( g
    , snd $ A.bounds g)

-- TODO: Extract to AoC
pairs :: [a] -> [(a, a)]
pairs l = go l l
  where go [] [] = []
        go (_:xs) [] = go xs xs
        go (x:xs) (y:ys) = (x, y):go (x:xs) ys

antinodes :: [((N, N), a)] -> [V2 N]
antinodes l = do
  ((a, _), (b, _)) <- pairs l
  guard $ a /= b
  let av = v2 a
      bv = v2 b
      d  = av - bv
  [ bv + 2 * d,
    av - 2 * d ]

letters :: UArrayGrid Char -> [[((N, N), Char)]]
letters g = groupBy (\(_, v1) (_, v2) -> v1 == v2)
          $ sortBy (comparing snd)
          [(k, v) | (k, v) <- A.assocs g
                  , v /= '.'
                  ]

part1 :: Input -> Int
part1 (g, bounds) =
  length
  . Set.fromList
  . filter (inBounds bounds)
  . concatMap antinodes
  $ letters g


inBounds :: (N, N) -> V2 N -> Bool
inBounds (w, h) (V2 (x, y)) =
  0 <= x && x <= w && 0 <= y && y <= h

antinodes' :: (N, N) -> [((N, N), a)] -> [V2 N]
antinodes' bounds l = do
  ((a, _), (b, _)) <- pairs l
  guard $ a /= b
  let av = v2 a
      bv = v2 b
      d  = av - bv
  concat [ takeWhile (inBounds bounds) $ map (\k -> bv + fromInteger k * d) [0..]
         , takeWhile (inBounds bounds) $ map (\k -> av - fromInteger k * d) [0..]
         ]

part2 :: Input -> Int
part2 (g, bounds) =
  length
  . Set.fromList
  . concatMap (antinodes' bounds)
  $ letters g

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
