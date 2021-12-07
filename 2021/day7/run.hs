{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.List

parseAll :: String -> [Int]
parseAll input = read @[Int] $ "[" ++ input ++ "]"

align :: (Int -> Int -> Int) -> [Int] -> Int
align cost xs =
  minimum
  . map (alignTo cost xs)
  $ [minimum xs..maximum xs]

alignTo :: (Int -> Int -> Int) -> [Int] -> Int -> Int
alignTo cost xs i = sum $ map (cost i) xs

part1 :: [Int] -> Int
part1 = align cost
  where cost i x = abs (x - i)

part2 :: [Int] -> Int
part2 = align cost
  where cost i x = let d = abs (x - i) in d * (d + 1) `div` 2

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
