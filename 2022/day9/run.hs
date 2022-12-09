{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC

import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

type Move = (Char, N)

parse :: String -> Move
parse = \case (c:' ':rest) -> (c, read @Int rest)

parseAll :: String -> [Move]
parseAll = map parse  . lines

splat :: [Move] -> [Char]
splat = concatMap (\(c, s) -> replicate s c)

close :: V2 N -> V2 N -> Bool
close x y =
  let diff = abs (x - y)
  in all (< 2) diff

clampUnit :: N -> N
clampUnit x =
  max (-1) (min 1 x)

trace :: [Move] -> [(V2 N, V2 N)]
trace = scanl f (0, 0) . splat
  where u (V2 (x, y)) = v2 (x, y + 1)
        d (V2 (x, y)) = v2 (x, y - 1)
        r (V2 (x, y)) = v2 (x + 1, y)
        l (V2 (x, y)) = v2 (x - 1, y)
        dir = \case 
            'U' -> u
            'D' -> d
            'R' -> r
            'L' -> l

        f (h, t) next =
          let h' = dir next h
              t' = if close h' t
                   then t
                   else t + (clampUnit <$> (h' - t))
          in
            (h', t')

traceN :: Int -> [Move] -> [[V2 N]]
traceN n = scanl f (replicate n 0) . splat
  where u (V2 (x, y)) = v2 (x, y + 1)
        d (V2 (x, y)) = v2 (x, y - 1)
        r (V2 (x, y)) = v2 (x + 1, y)
        l (V2 (x, y)) = v2 (x - 1, y)
        dir = \case 
            'U' -> u
            'D' -> d
            'R' -> r
            'L' -> l

        f (h:nodes) next = 
          let h' = dir next h
          in
            h':go next h' nodes
        go next inFront =
          \case [] -> []
                (n:nodes) ->
                  let n' = if close inFront n
                           then n
                           else n + (clampUnit <$> (inFront - n))
                  in
                    n':go next n' nodes


part1 = length . Set.fromList . map snd . trace
part2 = length . Set.fromList . map last . traceN 10

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
