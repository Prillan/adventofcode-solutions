{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Control.Monad (replicateM)
import Data.List.Split (chunksOf)
import Data.Word (Word64)

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type N = Int

parse :: String -> N
parse = read @N . drop 28

parseAll :: String -> (N, N)
parseAll input =
  let [p1, p2] = map parse (lines input)
  in (p1, p2)

deterministic :: [N]
deterministic = cycle $ [1..9] ++ [0]

uninterleave :: [a] -> ([a], [a])
uninterleave = go
  where go =
          \case []  -> ([], [])
                [x] -> ([x], [])
                (x:y:rest) ->
                  let (xs, ys) = go rest
                  in (x:xs, y:ys)

play :: N -> [[N]] -> [(N, Int)]
play start = scanl f (start, 0)
  where f (!pos, !s) d =
          let r = turn d pos
          in (r, s + r)

turn :: [N] -> N -> N
turn die start =
  let end = start + (sum die `mod` 10)
  in if end > 10 then end - 10 else end

part1 :: (N, N) -> Int
part1 (p1, p2) =
  let (p1d, p2d) = uninterleave $ chunksOf 3 deterministic
      p1s = takeWhile ((< 1000) . snd) $ play p1 p1d
      p2s = takeWhile ((< 1000) . snd) $ play p2 p2d
  in
    case (length p1s, length p2s) of
      (l1, l2) | l1 <= l2  -> (2 * l1 - 1) * 3 * (snd $ p2s !! (l1 - 1))
               | otherwise -> (snd $ p1s !! l2) *  2 * l2 * 3

dirac :: [N]
dirac =
  map sum
  $ replicateM 3 [1, 2, 3]

turn2 :: Vector [(N, Word64)]
turn2 =
  V.fromList . map outcomes $ [1..10]
  where outcomes s = Map.toList
                     . Map.fromListWith (+)
                     . map (, 1)
                     $ map (f s) dirac
        f s d =
          let end = s + d
          in if end > 10 then end - 10 else end


play2 :: (N, N) -> Map Bool Word64
play2 (p1, p2) = Map.fromListWith (+) $ go p1 0 p2 0 1
  where go !p1 !p1s !p2 !p2s !c = do
          (r1, c1) <- turn2 V.! (p1 - 1)
          if p1s + r1 >= 21
            then pure (True, c * c1)
            else do
              (r2, c2) <- turn2 V.! (p2 - 1)
              if p2s + r2 >= 21
                then pure (False, c * c1 * c2)
                else go r1 (p1s + r1) r2 (p2s + r2) (c * c1 * c2)

part2 :: (N, N) -> Word64
part2 = maximum . play2

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
