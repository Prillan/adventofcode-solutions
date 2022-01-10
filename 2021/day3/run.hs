{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Maybe (mapMaybe)
import Data.List (transpose)

parseAll :: String -> [String]
parseAll = lines

gamma :: [String] -> Int
gamma = readBinary . map mostCommon . transpose

epsilon :: [String] -> Int
epsilon = readBinary . map leastCommon . transpose

part1 :: [String] -> Int
part1 input = gamma input * epsilon input

locate :: ([Char] -> Char) -> [String] -> String
locate selector = go
  where go = \case
          [] -> []
          xs -> let m = selector (map head xs)
                in m:go (mapMaybe (pickSimilar m) xs)
        pickSimilar m =
          \case [_]           -> Nothing
                x:xs | x == m -> Just xs
                _             -> Nothing

oxyGen :: [String] -> Int
oxyGen = readBinary . locate mostCommon

co2Scrub :: [String] -> Int
co2Scrub = readBinary . locate leastCommon

part2 :: [String] -> Int
part2 input = oxyGen input * co2Scrub input

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
