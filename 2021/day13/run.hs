{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid
import AoC.Draw.Chars

import Data.Bifunctor
import Data.Foldable
import Data.List

type Paper = [(Int, Int)]

parsePoint :: String -> (Int, Int)
parsePoint l = read @(Int, Int) $ "(" ++ l ++ ")"

parseFold :: String -> (Int, Int)
parseFold f = case stripPrefix "fold along " f of
                Just ('x':'=':v) -> (read @Int v, 0)
                Just ('y':'=':v) -> (0, read @Int v)

parseAll :: String -> (Paper, [(Int, Int)])
parseAll input =
  let (points, folds) = break null (lines input)
  in (map parsePoint points, map parseFold (drop 1 folds))

paper :: Paper -> [[Bool]]
paper points =
    let (cs, rs) = unzip points
        cmax = maximum cs
        rmax = maximum rs
        cmin = minimum cs
        rmin = minimum rs
    in [ [ (ci, ri) `elem` points | ci <- [cmin..cmax] ]  | ri <- [rmin..rmax] ]

foldLine :: Int -> Paper -> Paper
foldLine x = map (first f)
  where f = \case i | i > x -> 2 * x - i
                    | otherwise -> i

swap :: (Int, Int) -> (Int, Int)
swap (x, y) = (y, x)

paperFold :: (Int, Int) -> Paper -> Paper
paperFold =
  \case (x, 0) -> nub . foldLine x
        (0, y) -> nub . map swap . foldLine y . map swap

pp :: [(Int, Int)] -> String
pp = ppGrid (\case True  -> 'X'
                   False -> ' ') . paper


part1 (points, f:_)   = length $ paperFold f points

part2 (points, folds) =
  (\case Right x -> x)
  . readLetters
  . pp
  . foldl' (flip paperFold) points $ folds

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   putStrLn (part2 input)
