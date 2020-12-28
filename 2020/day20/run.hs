{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

type Tile = [[Char]]

parseTile s =
  let h:rest = lines s
      tileId = read @Int . init . drop 5 $ h
  in (tileId, parseGrid id . unlines $ rest)

borders :: Tile -> [String]
borders = map head . symmetries

--            Tile  -> [Tile]
symmetries :: [[a]] -> [[[a]]]
symmetries tile = [s' (r' tile) | r' <- [id, r, r . r, r . r . r]
                                , s' <- [id, s]]
  where r = rotate
        s = reverse


-- CW
rotate :: [[a]] -> [[a]]
rotate = transpose . reverse

-- CCW
rotate' :: [[a]] -> [[a]]
rotate' = reverse . transpose

leftBorder :: Tile -> String
leftBorder = head . transpose

rightBorder :: Tile -> String
rightBorder = head . rotate'

bottomBorder :: Tile -> String
bottomBorder = last

topBorder :: Tile -> String
topBorder = head

corners :: Map Int Tile -> [Int]
corners tiles =
  let unmatched = unmatchedBorders tiles
      bts =
        Map.filter ((== 4) . length . filter (`elem` unmatched))
        $ borderMap tiles
  in Map.keys $ bts

borderMap = Map.map borders

unmatchedBorders =
  Map.keys
  . Map.filter (== 1)
  . counter
  . concat
  . Map.elems
  . borderMap

assemble :: Map Int Tile -> [[Tile]]
assemble tiles =
  let unmatched = unmatchedBorders tiles
      tl = tiles Map.! tlid
      tlid = head $ corners tiles
      isTopLeft x =
        all (`elem` unmatched)
        $ map head $ [x, transpose x]
      -- Just pick a tile to place in the top-left
      tl' = head $ filter isTopLeft . symmetries $ tl

      -- TODO: Performance optimization, only consider tiles that
      -- haven't been used yet.
      findNext bd0 bd1 (tid, tile) =
        let rbd = bd0 tile
        in find (\(_, t) -> bd1 t == rbd)
           . concatMap (\(i, t) -> (i,) <$> symmetries t)
           . Map.toList
           $ Map.delete tid tiles

      findNextInRow = findNext rightBorder leftBorder
      findNextInCol = findNext bottomBorder topBorder

      complete f (id0, tl0) =
        mapMaybe id
        . takeWhile isJust
        $ iterate (>>= f) (Just (id0, tl0))

      completeRow = complete findNextInRow
      completeCol = complete findNextInCol

      row0 = completeRow (tlid, tl')
  in map (map snd) $ map completeCol row0

glue :: [[Tile]] -> Tile
glue =
  -- lul
  concat . map (transpose . concat . map (init . tail . map (init . tail)))

seaMonster :: [[Char]]
seaMonster =
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   " ]

numberOfSeaMonsters :: Tile -> Int
numberOfSeaMonsters =
  head
  . filter (> 0)                           -- Pick the first one with
                                           -- monsters
  . map (length . filter (== 15) . concat) -- How many?
  . map (convolve (intify seaMonster))     -- Search each image
  . symmetries                             -- Try all symmetries
  . intify


-- Converts # to 1 and . to 0
intify :: Tile -> [[Int]]
intify = map (map fromEnum . map (== '#'))

-- Convolution of (non-flipped) kernel on image.
convolve :: [[Int]] -> [[Int]] -> [[Int]]
convolve kernel image =
  map f (overlappingChunks ky image)
  where kx = length $ head kernel
        ky = length kernel
        f rowChunk = map (match kernel)
                     . transpose
                     $ map (overlappingChunks kx) rowChunk

overlappingChunks i = filter ((== i) . length) . map (take i) . tails

match kernel = sum . concat . zip2dWith (*) kernel

-- 2-dimensional zipWith
zip2dWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zip2dWith op = zipWith (zipWith op)

parseAll =
  Map.fromList
  . map parseTile
  . filter (not . null)
  . splitOn "\n\n"

part1 :: Map Int Tile -> Int
part1 = product . corners

part2 :: Map Int Tile -> Int
part2 tiles =
  let image = glue (assemble tiles)
      n = numberOfSeaMonsters image
      roughTiles = length . filter (== '#') . concat $ image
  in roughTiles - (15 * n)

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
