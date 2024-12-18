{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC.Search

import Control.Monad (guard)
import Data.List (inits)
import Data.Maybe (fromJust)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

type N = Int
type Input = [(N, N)]

parse :: String -> (N, N)
parse = read . ("(" <>) . (<> ")")

parseAll :: String -> Input
parseAll = map parse  . lines

shortest :: HashSet (N, N) -> (N, N) -> Maybe Int
shortest grid goal@(r, b) = bfs_ (== goal) next (0, 0)
  where next (x, y) = do
          p@(x', y') <- [ (x + 1, y    )
                        , (x - 1, y    )
                        , (x    , y + 1)
                        , (x    , y - 1)
                        ]
          guard $ 0 <= x' && x' <= r
          guard $ 0 <= y' && y' <= b
          guard $ not $ p `HashSet.member` grid
          pure p

part1 :: Input -> Int
part1 input = fromJust $ shortest grid goal
  where grid = HashSet.fromList (take 1024 input)
        goal = ( maximum $ map fst input
               , maximum $ map snd input
               )

bisect :: Integral a => (a -> Ordering) -> a -> a -> a
bisect check low high = go low high
  where go l h | l == h    = l
               | otherwise =
                   let x = (h + l) `div` 2
                   in case check x of
                        LT | l == x    -> l
                           | otherwise -> go x h
                        EQ -> go l x
                        GT -> go l x

part2 :: Input -> (N, N)
part2 input = input !! bisect check 0 (length input)
  where grids = map HashSet.fromList $ inits input
        goal = ( maximum $ map fst input
               , maximum $ map snd input
               )
        check n = case single n of
                    Just _  -> LT
                    Nothing -> GT
        single n = shortest (grids !! n) goal

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

printTuple :: (Show a, Show b) => (a, b) -> IO ()
printTuple (x, y) = putStrLn $ show x ++ "," ++ show y

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   printTuple (part2 input)
