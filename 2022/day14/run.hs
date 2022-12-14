{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
import AoC

import Data.Foldable (find, toList)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

type N = Int
type Path = [(N, N)]

parse :: String -> Path
parse = map f . splitOn " -> "
  where f s = read $ "(" ++ s ++ ")"

parseAll :: String -> [Path]
parseAll = map parse  . lines

draw :: Path -> [(N, N)]
draw nodes = concat $ zipWith d nodes (drop 1 nodes)
  where d (fx, fy) (tx, ty)
          | fx == tx = map (fx,) [min fy ty..max fy ty]
          | fy == ty = map (,fy) [min fx tx..max fx tx]
          | otherwise = error "segment is diagonal!"

gridify :: [Path] -> HashSet (N, N)
gridify =
  HashSet.fromList
  . concatMap draw

point :: (N, N)
point = (500, 0)

singleFloor :: N -> HashSet (N, N) -> Maybe (N, N)
singleFloor maxY g = go point
  where go p@(x, y)
          | y == maxY  = Just (x, y)
          | not ((x, y+1) `HashSet.member` g) = go (x, y+1)
          | not ((x-1,y+1) `HashSet.member` g) = go (x-1,y+1)
          | not ((x+1,y+1)  `HashSet.member` g) = go (x+1,y+1)
          | p == point = Nothing
          | otherwise = Just (x, y)

singleVoid :: HashSet (N, N) -> Maybe (N, N)
singleVoid g = go point
  where go (x, y)
          | y > 1000 = Nothing
          | not ((x, y+1) `HashSet.member` g) = go (x, y+1)
          | not ((x-1,y+1) `HashSet.member` g) = go (x-1,y+1)
          | not ((x+1,y+1)  `HashSet.member` g) = go (x+1,y+1)
          | otherwise = Just (x, y)

step :: (HashSet (N, N) -> Maybe (N, N))
     -> Maybe (HashSet (N, N))
     -> Maybe (HashSet (N, N))
step single g = do
  g' <- g
  p  <- single g'
  pure $ HashSet.insert p g'

simulate :: (HashSet (N, N) -> Maybe (N, N)) -> HashSet (N, N) -> Int
simulate single =
  fst
  . fromJust
  . find (not . isJust . snd)
  . zip [0..]
  . iterate (step single)
  . Just

part1 :: [Path] -> Int
part1 = (\x -> x - 1) . simulate singleVoid . gridify

part2 :: [Path] -> Int
part2 paths =
  let g = gridify paths
      maxY = maximum . map snd $ toList g
  in
    simulate (singleFloor (maxY + 1)) g

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
