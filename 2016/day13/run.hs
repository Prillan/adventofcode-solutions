{-# LANGUAGE TupleSections #-}
import AoC.Search (bfs_)
import Control.Monad (guard)
import Data.Bits
import Data.Maybe (fromJust)

f x y = x*x + 3*x + 2*x*y + y + y*y

wall :: (Bits a, Num a) => a -> a -> a -> Bool
wall input x y = odd (popCount (f x y + input))

drawCell input x y =
  if wall input x y
    then '#'
    else '.'

printGrid :: Int -> Int -> Int -> String
printGrid input w h =
  unlines [[drawCell input x y | x <- [0..w]] | y <- [0..h]]

woop :: [(a -> a)] -> (a, a) -> [(a, a)]
woop fs (x, y) = do
  f <- fs
  [(f x, y), (x, f y)]

neighboursOf :: Integer -> (Integer, Integer) -> [(Integer, Integer)]
neighboursOf input pos = do
  (x', y') <- woop [(+1), (+ negate 1)] pos
  guard $ x' >= 0 && y' >= 0
  guard $ not $ wall input x' y'
  pure (x', y')

minCost input target = bfs_ (== target) (neighboursOf input) (1, 1)

part1 input =
  fromJust $ minCost input (31, 39)

-- TODO: Limit bfs search to stop after 50 steps.
part2 input = length $ do
  target <- [(x, y) | x <- [0..50], y <- [0..50], x + y <= 50]
  case minCost input target of
    Just steps -> do
      guard $ steps <= 50
      pure target
    Nothing -> []

main = do
  let input = 1358
  print (part1 input)
  print (part2 input)
