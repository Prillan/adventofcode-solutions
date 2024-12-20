{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid.New as Grid

import Data.Array.IArray
import Control.Monad (guard)
import Data.List (isPrefixOf)
import Data.Maybe

import qualified Data.HashMap.Strict as HashMap

type Input = (UArrayGrid Char, (Int, Int))

parseAll :: String -> Input
parseAll input =
  let g = Grid.parse id input
  in (g, snd $ bounds g)

dirs :: [V2 Int]
dirs =
  [ v2 (cd, rd) | rd <- [-1, 0, 1]
                , cd <- [-1, 0, 1]
                , not (rd == 0 && cd == 0)
                ]

star :: UArrayGrid Char -> (Int, Int) -> [String]
star g pos = do
  let pos' = v2 pos
  dir <- dirs
  pure
    . catMaybes
    . takeWhile isJust
    . map (Grid.lookup g)
    . map (\(V2 x) -> x)
    $ map (\i -> pos' + fromInteger i * dir) [0::Integer ..]

part1 :: Input -> Int
part1 (g, (xm, ym)) = length do
  x <- [0..xm]
  y <- [0..ym]
  v <- star g (x, y)
  guard $ "XMAS" `isPrefixOf` v
  pure ()

part2 :: Input -> Int
part2 (g, (xm, ym)) = length do
  x <- [0..xm]
  y <- [0..ym]
  let diag1 = mapMaybe (Grid.lookup g) [(x-1, y-1), (x, y), (x+1, y+1)]
      diag2 = mapMaybe (Grid.lookup g) [(x-1, y+1), (x, y), (x+1, y-1)]
  guard $ diag1 == "MAS" || diag1 == "SAM"
  guard $ diag2 == "MAS" || diag2 == "SAM"
  pure ()

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
