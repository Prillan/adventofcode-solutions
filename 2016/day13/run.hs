{-# LANGUAGE TupleSections #-}
import           Control.Monad (guard)
import           Data.Bits
import           Data.Set (Set)
import qualified Data.Set as Set


data Queue a = Queue [a] [a]
newQueue = Queue [] []
enq (Queue xs ys) y = Queue xs (y:ys)
deq (Queue [] []) = Nothing
deq (Queue (x:xs) ys) = Just (x, Queue xs ys)
deq (Queue [] ys) = deq (Queue (reverse ys) [])

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

bfs :: (Ord a, Eq a) => a -> (a -> [a]) -> (a -> Bool) -> Maybe (Int, a)
bfs state neighbours done = bfs' Set.empty (enq newQueue (0, state))
  where bfs' visited queue = do
          ((steps, current), queue') <- deq queue
          let visited' = Set.insert current visited
              valid s = not (Set.member s visited')
              queue'' = foldr (flip enq) queue'
                        . map (steps+1,)
                        . filter valid
                        . neighbours $ current
          if done current
            then pure (steps, current)
            else bfs' visited' queue''

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

part1Input = 1358
part1 =
  bfs (1, 1) (neighboursOf part1Input) (== (31, 39))

-- Hacky solution!
-- Runs in 5 seconds on my laptop.
part2 = length $ do
  target <- [(x, y) | x <- [0..50], y <- [0..50], x + y <= 50]
  case bfs (1, 1) (neighboursOf part1Input) (== target) of
    Just (steps, _) -> do
      guard $ steps <= 50
      pure target
    Nothing -> []

main = do
  print part1
  print part2
