{-# LANGUAGE BlockArguments #-}

import Data.List (transpose)
import Data.List.Split (chunksOf, splitWhen)
import Data.Maybe (fromJust)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Move = (Int, Int, Int)
type Crate = Char
type Stacks = Map Int [Crate]

parseStacks :: [String] -> Stacks
parseStacks =
  Map.fromList
  . zip [1..]
  . map (filter (/= ' '))
  . transpose
  . map (map (!! 1) . chunksOf 4)

parseMoves :: [String] -> [Move]
parseMoves = map f . filter (not . null)
  where f move =
          let [_, count, _, from, _, to] = map read $ words move
          in
            (count, from, to)

parseAll :: String -> (Stacks, [Move])
parseAll input =
  let [stacks, moves] = splitWhen ((== " 1") . take 2) $ lines input
  in (parseStacks stacks, parseMoves moves)

exec :: ([Crate] -> [Crate]) -> Stacks -> Move -> Stacks
exec lift stacks (c, from, to) = fromJust do
  fs <- stacks Map.!? from
  ts <- stacks Map.!? to

  let (move, fs') = splitAt c fs
      ts' = lift move ++ ts

  pure $ Map.insert from fs' $  Map.insert to ts' $ stacks

tops :: Stacks -> [Crate]
tops = map (head . snd) . Map.toAscList

part1 :: (Stacks, [Move]) -> String
part1 (stacks, moves) = tops $ foldl (exec reverse) stacks moves

part2 :: (Stacks, [Move]) -> String
part2 (stacks, moves) = tops $ foldl (exec id) stacks moves

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   putStrLn (part1 input)
   putStrLn (part2 input)
