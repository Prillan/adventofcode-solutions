{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC.Grid

import Control.Monad (guard)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set

type N = Int
type Input = MapGrid N

parseAll :: String -> Input
parseAll = parseMapGrid (read @N . pure)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) =
  [ (x - 1, y    )
  , (x + 1, y    )
  , (x    , y - 1)
  , (x    , y + 1)
  ]

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

trails :: Input -> (Int, Int) -> [(Int, Int)]
trails g sp = go 0 [sp]
  where go 9 ps = ps
        go i ps =
          let next = do
                p <- ps
                p' <- neighbors p
                Just j <- pure $ g HashMap.!? p'
                guard $ j == i + 1
                pure p'
          in
            go (i + 1) next

startPoints :: Input -> [(Int, Int)]
startPoints = map fst . filter ((== 0) . snd) . HashMap.toList

part1 :: Input -> Int
part1 g = sum . map (length . ordNub) $ map (trails g) (startPoints g)

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
