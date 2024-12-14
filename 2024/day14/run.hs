{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Ix (range)
import Data.Foldable (forM_)
import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type N = Integer

type Input = [(V2 N, V2 N)]

parse :: String -> (V2 N, V2 N)
parse input =
  let [p, v] = map (v2 . read . (\x -> "(" <> x <> ")") . drop 2) $ words input
  in  (p, v)

parseAll :: String -> Input
parseAll = map parse  . lines

bounds :: V2 N
bounds = v2 (101, 103)

maxIter :: N
maxIter = product bounds

within :: (N, N) -> (N, N) -> V2 N -> Bool
within (lx, ly) (hx, hy) (V2 (x, y)) =
  lx <= x && x < hx && ly <= y && y < hy

stepN :: N -> V2 N -> V2 N -> V2 N
stepN n p v = mod <$> (p + fromIntegral n * v) <*> bounds

safetyFactor :: [V2 N] -> Int
safetyFactor xs = product $ map (\(ql, qh) -> length $ filter (within ql qh) xs) quadrants
  where V2 (bw, bh) = bounds
        bw' = bw `div` 2
        bh' = bh `div` 2
        quadrants = [ ((      0,       0), (bw', bh'))
                    , ((bw' + 1,       0), (bw , bh'))
                    , ((      0, bh' + 1), (bw', bh ))
                    , ((bw' + 1, bh' + 1), (bw , bh ))
                    ]

part1 :: Input -> Int
part1 = safetyFactor . map (uncurry (stepN 100))

part2 :: Input -> IO ()
part2 input = do
  let ((i, _, g), _) = interesting input
  print i
  -- printGrid g

emptyGrid :: MapGrid Char
emptyGrid = HashMap.fromList $ map (,'.') (range ((0, 0), (fromInteger w, fromInteger h)))
  where V2 (w, h) = bounds

printGrid :: [V2 N] -> IO ()
printGrid = putStrLn . ppMapGrid id . toGrid

toGrid :: [V2 N] -> MapGrid Char
toGrid g = rs `HashMap.union` emptyGrid
  where rs = HashMap.fromList $ map (\(V2 (x, y)) -> ((fromInteger x, fromInteger y), '#')) g

interesting :: Input -> ( (N, Int, [V2 N])
                        , (N, Int, [V2 N])
                        )
interesting input = ( minimumBy (comparing (\(_, y, _) -> y)) grids
                    , maximumBy (comparing (\(_, y, _) -> y)) grids
                    )
  where grids = map (\i -> let g = map (uncurry (stepN i)) input
                           in (i, safetyFactor g, g)) [0..maxIter]

iterPrint :: Input -> IO ()
iterPrint input = forM_ [0..maxIter] \i -> do
  print i
  printGrid $ map (uncurry (stepN i)) input
  print "---"

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   part2 input
--   iterPrint input
