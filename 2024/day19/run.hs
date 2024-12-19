{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC.Search (bfs_)

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.String (fromString)
import Data.Word (Word8)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

type N = Int
type S = ByteString

type Input = ([S], [S])

parseTargets :: String -> [S]
parseTargets = map fromString . lines

parseTowels :: String -> [S]
parseTowels =
  map fromString
  . sortBy (comparing (\t -> (negate $ length t, t)))
  . filter (not . null)
  . map (filter (\c -> 'a' <= c && c <= 'z'))
  . splitOn ","

parseAll :: String -> Input
parseAll input =
  let [towels, targets] = splitOn "\n\n" input
  in (parseTowels towels, parseTargets targets)

solve :: [S] -> S -> Maybe Int
solve towels t = bfs_ BS.null (\n -> mapMaybe (`BS.stripPrefix` n) towels) t

search :: [S] -> S -> HashMap Int IntSet
search towels t = search' (map vectorize (reverse towels)) (vectorize t)

search' :: [Vector Word8] -> Vector Word8 -> HashMap Int IntSet
search' towels t = go (HashMap.singleton 0 IntSet.empty) (PQueue.singleton 0 0)
  where target = V.length t
        go visited q =
          case PQueue.minView q of
            Nothing -> visited
            Just (i, is) | i == target -> go visited is
                         | otherwise   ->
                           let v = V.drop i t
                               nexts = filter (\(c, l) -> V.take l v == c)
                                       $ map (\c -> (c, V.length c)) towels
                               nexts' = filter (<= target) $ map (\(_, l) -> l + i) nexts
                               nexts'' = filter (not . (`HashMap.member` visited)) nexts'
                               asdf = IntSet.singleton i
                               vis' = HashMap.unionWith (<>) visited (HashMap.fromList $ map (,asdf) $ nexts')
                               q' = PQueue.fromList $ map (\j -> (j - i, j)) nexts''
                           in go vis' (is <> q')

traceBack :: HashMap Int IntSet -> Int -> Maybe N
traceBack tr e = go e
  where memoized = map go [0..e]
        go = \case 0 -> Just 1
                   x | Just ys <- tr HashMap.!? x -> sum <$> traverse (memoized !!) (IntSet.toList ys)
                     | otherwise -> Nothing

vectorize :: ByteString -> Vector Word8
vectorize = V.fromList . BS.unpack

part1 :: Input -> Int
part1 (towels, targets) = length $ mapMaybe (solve towels) targets

part2 :: Input -> N
part2 (towels, targets) = sum $ mapMaybe single targets
  where single t = let r = search towels t
                   in traceBack r (BS.length t)

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
