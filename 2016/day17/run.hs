{-# LANGUAGE TupleSections #-}
import           Control.Monad (guard)
import           Data.List (permutations, group, minimum, maximum, minimumBy, maximumBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Megaparsec hiding (State)

import           Data.ByteString.Char8 (ByteString, pack, unpack)
import           Crypto.Hash

md5 :: ByteString -> Digest MD5
md5 = hash

data Queue a = Queue [a] [a]
newQueue = Queue [] []
fromList = flip Queue []
toList (Queue front back) = front ++ reverse back
enq (Queue xs ys) y = Queue xs (y:ys)
deq (Queue [] []) = Nothing
deq (Queue (x:xs) ys) = Just (x, Queue xs ys)
deq (Queue [] ys) = deq (Queue (reverse ys) [])

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

type State = (String, (Int, Int))

type Input = String

woop :: [(a -> a)] -> (a, a) -> [(a, a)]
woop fs (x, y) = do
  f <- fs
  [(f x, y), (x, f y)]

done :: State -> Bool
done (path, (x, y))
 | x == 3 && y == 3 = True
 | otherwise = False

dirs :: Int -> Char
dirs 0 = 'U'
dirs 1 = 'D'
dirs 2 = 'L'
dirs 3 = 'R'
dirs x = error $ "Invalid dir: " ++ show x

neighboursOf :: Input -> State -> [State]
neighboursOf input (path, (x, y))
 | (x, y) == (3, 3) = []
 | otherwise = do
  (i, (x', y')) <- zip [0..] [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
  guard $ 0 <= x' && x' < 4 && 0 <= y' && y' < 4
  let h = take 4 . show $ md5 (pack $ input ++ reverse path)
  guard $ (h !! i) `elem` "bcdef"
  pure (dirs i : path, (x', y'))

allDone input =
  filter done
  . concat
  . takeWhile (not.null)
  $ iterate (>>= neighboursOf input) [("", (0, 0))]

part1 input = do
  (steps, (path, _)) <- bfs ("", (0, 0)) (neighboursOf input) done
  pure (steps, reverse path)
part2 = length . fst . last . allDone

main = do
  let input = "pslxynzg"
  print (part1 input)
  print (part2 input)
