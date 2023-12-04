{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import Data.List (intersect, stripPrefix)
import Data.List.Split

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type N = Int

nums :: String -> [N]
nums = map (read @N) . words

parse :: String -> Card
parse input =
  let [cs, numbers] = splitOn ": " input
      [winnings, yours] = splitOn " | " numbers
      Just idx = dropWhile (== ' ') <$> "Card" `stripPrefix` cs
  in
    (read idx, (nums winnings, nums yours))

type Card = (Int, ([N], [N]))

parseAll :: String -> [Card]
parseAll = map parse . lines

wins :: ([N], [N]) -> Int
wins (w, draw) =
  length $ w `intersect` draw

score :: Card -> N
score (_, c) =
  case wins c of
    0 -> 0
    n -> 2 ^ (n - 1)

part1 :: [Card] -> N
part1 =
  sum . map score

ones :: [N] -> HashMap N Int
ones = HashMap.fromList . map (, 1)

part2 :: [Card] -> Int
part2 cs = sum $ go (ones $ map fst cs) cs
  where go counts = \case
          [] -> counts
          (i, c):rest ->
            let w = wins c
                Just n = counts HashMap.!? i
                winners = HashMap.map (*n) . ones $ map (+i) [1..w]
                counts' = HashMap.unionWith (+) counts winners
            in go counts' rest

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
