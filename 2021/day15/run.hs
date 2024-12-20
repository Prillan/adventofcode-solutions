{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC.Grid.New as Grid
import AoC.Search (dijkstra_)

import Data.List (transpose)
import qualified Data.Array.IArray as A

type N = Int
type Input = [[N]]

parseAll :: String -> Input
parseAll = readGrid

minPathCost :: UArrayGrid N -> Int
minPathCost grid =
  let (_, br) = A.bounds grid
      Just cost = dijkstra_ (br ==)
                            (Grid.ihvneighbors grid)
                            (0, 0)
  in cost

extend :: Int -> [N] -> [N]
extend = go
  where go 0 xs = xs
        go n xs = xs ++ map (\case i | i == 9    -> 1
                                     | otherwise -> i + 1) (go (n - 1) xs)

part1 :: Input -> Int
part1 = minPathCost . toUArrayGrid

part2 :: Input -> Int
part2 =
  minPathCost
  . toUArrayGrid
  . transpose
  . map (extend 4)
  . transpose
  . map (extend 4)

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
