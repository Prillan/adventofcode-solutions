{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC

import Data.Foldable ( Foldable(toList) )
import Data.List ( partition )
import Data.List.Split ( splitOn )

import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

v3FromList :: [a] -> V3 a
v3FromList [x,y,z] = v3 (x, y, z)

parse :: String -> V3 N
parse = v3FromList . map (read @N) . splitOn ","

parseAll :: String -> [V3 N]
parseAll = map parse  . lines

connected :: [V3 N] -> [(V3 N, V3 N)]
connected nodes =
  let nset = Set.fromList nodes
  in
    [ (c1, c2)
    | c1 <- nodes
    , c2 <- hvNeighbors3 c1
    , c2 `Set.member` nset
    ]

part1 :: [V3 N] -> Int
part1 nodes =
  let conn = length (connected nodes)
  in length nodes * 6 - conn

side :: N -> [V2 N]
side b = [ v2 (x, y)
         | x <- [0..b]
         , y <- [0..b]
         ]

hvNeighbors3 :: V3 N -> [V3 N]
hvNeighbors3 n =
  [ n + v3 (1, 0, 0)
  , n + v3 (-1, 0, 0)
  , n + v3 (0, 1, 0)
  , n + v3 (0, -1, 0)
  , n + v3 (0, 0, 1)
  , n + v3 (0, 0, -1)
  ]

fill :: N -> [V3 N] -> Int
fill b nodes = go (Set.fromList initial) 0 initial
  where nset = Set.fromList nodes
        go !visited !acc =
          \case
            [] -> acc
            n:rest ->
              let (hits, nexts) = partition (`Set.member` nset)
                                  . filter (valid visited)
                                  $ hvNeighbors3 n
              in
                go (Set.fromList nexts `Set.union` visited) (acc + length hits) (nexts ++ rest)

        valid visited n =
          all (\c -> c >= 0 && c <= b) n
          && not (n `Set.member` visited)

        initial = map (extendX' (-1)) (side b)
                  ++ map (extendX' (b + 1)) (side b)
                  ++ map (extendY' (-1)) (side b)
                  ++ map (extendY' (b + 1)) (side b)
                  ++ map (extendZ' (-1)) (side b)
                  ++ map (extendZ' (b + 1)) (side b)

part2 :: [V3 N] -> Int
part2 nodes =
  let b = maximum $ concatMap toList nodes
  in fill b nodes

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
