{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC.Grid.New as Grid

import Control.Monad (guard)

import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set

type N = Int
type Input = UArrayGrid N

parseAll :: String -> Input
parseAll = Grid.parse (read . pure)

trails :: Input -> (Int, Int) -> [(Int, Int)]
trails g sp = go 0 [sp]
  where go 9 ps = ps
        go i ps =
          let next = do
                p <- ps
                (p', j) <- Grid.ihvneighbors g p
                guard $ j == i + 1
                pure p'
          in
            go (i + 1) next

startPoints :: Input -> [(Int, Int)]
startPoints = map fst . filter ((== 0) . snd) . Grid.toList

part1 :: Input -> Int
part1 g = sum . map (length . nubOrd) $ map (trails g) (startPoints g)

part2 :: Input -> Int
part2 g = sum . map length $ map (trails g) (startPoints g)

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
