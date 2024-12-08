{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Data.List.Split (splitOn)

type N = Int

type Row = (N, [N])
type Input = [Row]

parse :: String -> Row
parse input =
  let [x, xs] = splitOn ":" input
  in (read x, map read $ words xs)

parseAll :: String -> Input
parseAll = map parse  . lines

type PreppedRow = (N, [(N, N)])

prep :: Row -> PreppedRow
prep (t, xs) = (t, zip xs factors)
  where factors = map ((10 ^) . length . show) xs

solve :: Bool -> PreppedRow -> [()]
solve conc (t, nums) = go 0 False nums
  where go v _ [] | v == t    = [()]
                  | otherwise = []
        go v canConc ((x, f):xs)
          = go (x + v) True xs
            ++ go (x * v) True xs
            ++ (if conc && canConc then go (v * f + x) True xs else [])

solveAll :: Bool -> Input -> [N]
solveAll conc = map fst . filter (not . null . solve conc . prep)

part1 :: Input -> N
part1 = sum . solveAll False

part2 :: Input -> N
part2 = sum . solveAll True

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
