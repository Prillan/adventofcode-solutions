module Main (main) where

import Data.List ( zipWith4 )

validPart1 :: Int -> Bool
validPart1 x =
  let s = show x
  in
    or (zipWith (==) s (drop 1 s))
    && and (zipWith (<=) s (drop 1 s))

validPart2 :: Int -> Bool
validPart2 x =
  let s = '.':show x ++ ":"
  in
    or (zipWith4
         (\a b c d -> b == c && a /= b && c /= d)
         s
         (drop 1 s)
         (drop 2 s)
         (drop 3 s))
    && and (zipWith (<=) s (drop 1 s))


part1 :: (Int, Int) -> Int
part1 (low, high) =
  length . filter validPart1 $ [low..high]

part2 :: (Int, Int) -> Int
part2 (low, high) =
  length . filter validPart2 $ [low..high]

main :: IO ()
main = do
   let input = (372304, 847060)
   print (part1 input)
   print (part2 input)
