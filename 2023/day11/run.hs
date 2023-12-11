{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
import AoC.Grid

import Data.List (findIndices, sort, tails, transpose)

import qualified Data.HashMap.Strict as HashMap

parseAll :: String -> [[Char]]
parseAll = lines

compressed :: [[Char]] -> [Int]
compressed = findIndices (all (== '.'))

hitComp :: [Int] -> Int -> Int -> Int
hitComp cs from to = length $ filter p cs
  where p c = from <= c && c < to

dist :: Int -> [Int] -> Int -> Int -> Int
dist k cs from to =
  to - from + (k - 1) * hitComp cs from to

shortest :: Int -> [Int] -> [Int] -> (Int, Int) -> (Int, Int) -> Int
shortest k cs rs (fc, fr) (tc, tr) =
  let [fc', tc'] = sort [fc, tc]
      [fr', tr'] = sort [fr, tr]
  in
    dist k cs fc' tc' + dist k rs fr' tr'

-- TODO: move to AoC
pairs :: [a] -> [(a, a)]
pairs xs = do
  x:rest <- tails xs
  y <- rest
  pure (x, y)

solve :: Int -> [[Char]] -> Int
solve k g =
  let rs = compressed g
      cs = compressed (transpose g)
      stars = HashMap.keys $ HashMap.filter (== '#') $ toMapGrid g
  in sum do
    (s1, s2) <- pairs stars
    pure $ shortest k cs rs s1 s2

part1, part2 :: [[Char]] -> Int
part1 = solve 2
part2 = solve 1_000_000

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
