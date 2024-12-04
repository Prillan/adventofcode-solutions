{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Control.Monad (guard)

import Data.List (isPrefixOf)
import Data.Maybe

import qualified Data.HashMap.Strict as HashMap

type Input = (MapGrid Char, (Int, Int))

parseAll :: String -> Input
parseAll input =
  let g = parseMapGrid id input
  in (g, maximum (HashMap.keys g))

dirs :: [V2 Int]
dirs =
  [ v2 (cd, rd) | rd <- [-1, 0, 1]
                , cd <- [-1, 0, 1]
                , not (rd == 0 && cd == 0)
                ]

star :: MapGrid Char -> (Int, Int) -> [String]
star g pos = do
  let pos' = v2 pos
  dir <- dirs
  pure
    . mapMaybe id
    . takeWhile isJust
    . map (g HashMap.!?)
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
  let diag1 = mapMaybe (g HashMap.!?) [(x-1, y-1), (x, y), (x+1, y+1)]
      diag2 = mapMaybe (g HashMap.!?) [(x-1, y+1), (x, y), (x+1, y-1)]
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
