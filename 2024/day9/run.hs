{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import Data.Foldable
import Data.List.Split (chunksOf)
import Data.Maybe

import Data.Word (Word16)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

type N = Word16
type Input = [(Int, Int)]

parse :: [Char] -> (Int, Int)
parse = \case
  [x, y] -> (read [x], read [y])
  [x] -> (read [x], 0)

parseAll :: String -> Input
parseAll =
  map parse . chunksOf 2 . takeWhile (/= '\n')

initialize :: Input -> Vector N
initialize input = V.concat do
    (i, (c, e)) <- zip [0 :: N ..] input
    [ V.replicate c i,
      V.replicate e maxBound ]

rearrange :: Vector N -> Vector N
rearrange v = V.modify (go empties nonEmpty) v
  where go (l:es) (r:nes) mv
          | l > r     = pure ()
          | otherwise = do
              MV.swap mv l r
              go es nes mv

        empties  = V.toList $ V.findIndices (== maxBound) v
        nonEmpty = reverse . V.toList $ V.findIndices (/= maxBound) v

checksum :: Vector N -> Int
checksum = V.sum . V.imap (\i -> \case v | v == maxBound -> 0
                                         | otherwise     -> i * fromIntegral v)

sliceSwap :: MV.Unbox a => Int -> Int -> Int -> Vector a -> Vector a
sliceSwap i j c = V.modify \mv ->
  forM_ [0..c-1] \k ->
    MV.swap mv (i + k) (j + k)

rearrange' :: Vector N -> Vector N
rearrange' v = go (reverse blocks) (firstEmpty 0 v) v
  where l = V.length v
        go [] _ u = u
        go ((i, c):bs) fe u =
          case filter (\j -> V.all (== maxBound) (V.slice j c u)) [fe .. min (l - c - 1) i] of
            j:_ -> let u' = sliceSwap i j c u
                   in go bs (if fe == j then firstEmpty fe u' else fe) u'
            []  -> go bs fe u

        firstEmpty curr = (+ curr) . fromJust . V.elemIndex maxBound . V.drop curr

        blocks =
          map (\(i, _, l) -> (i, l))
          . filter (\(_, val, _) -> val /= maxBound)
          . map (\u -> let (i, val) = V.head u
                       in (i, val, V.length u))
          . V.groupBy (\(_, a) (_, b) -> a == b)
          $ V.imap (,) v

part1 :: Input -> Int
part1 = checksum . rearrange . initialize

part2 :: Input -> Int
part2 = checksum . rearrange' . initialize

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
