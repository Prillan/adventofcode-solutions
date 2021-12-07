{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.List

parseAll :: String -> [Int]
parseAll input = read @[Int] $ "[" ++ input ++ "]"

align :: (Int -> Int -> Int) -> [Int] -> [Int] -> Int
align cost range xs =
  minimum
  . map (alignTo cost xs)
  $ range

alignTo :: (Int -> Int -> Int) -> [Int] -> Int -> Int
alignTo cost xs i = sum $ map (cost i) xs

-- part1 corresponds to finding the minimum of
--  f(x) = sum_y | y - x |
-- this is known to happen at x = median Y
part1 :: [Int] -> Int
part1 xs =
  let xs' = map fromIntegral xs
      m = median xs'
      range = [floor m..ceiling m]
      cost i x = abs (x - i)
  in align cost range xs

-- part2 corresponds to finding the minimum of
--  f(x) = sum_y |y-x|(|y-x|+1)/2
--       = sum_y (y-x)²/2 + sum_y |y-x|/2
--       = 1/2 * (sum_y (y-x)² + sum_y |y-x|)
--
--  sum_y (y-x)² has its minimum at x = mean Y
--  sum_y |y-x|  has its minimum at x = median Y
--
--  let a = min(mean Y, median Y)
--      b = max(mean Y, median Y)
--
--  both sums grow monotonically outside of the interval [a, b] which
--  implies that the convex function f has its global minimum in that
--  interval.
part2 :: [Int] -> Int
part2 xs =
  let xs' = map fromIntegral xs
      m1 = median xs'
      m2 = mean xs'
      [low, high] = sort [m1, m2]
      range = [floor low..ceiling high]
      cost i x = let d = abs (x - i) in d * (d + 1) `div` 2
  in align cost range xs

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
