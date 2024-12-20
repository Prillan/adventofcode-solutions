{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module AoC.Grid.New where

import Prelude hiding (lookup)

import AoC (V2(..), v2)
import Data.Word (Word8)
import Data.List (groupBy, transpose)
import Data.Maybe (catMaybes, isJust, mapMaybe)

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import Data.Array (Array)
import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as A
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA

class Grid g where
  type Cell g
  toLists :: g -> [[Cell g]]
  lookup :: g -> (Int, Int) -> Maybe (Cell g)
  parse :: (Char -> Cell g) -> String -> g
  toList :: g -> [((Int, Int), Cell g)]

neighbors :: Grid g => g -> (Int, Int) -> [Cell g]
neighbors g pos = mapMaybe snd $ ineighbors' g pos
{-# SPECIALIZE neighbors :: MapGrid Char -> (Int, Int) -> [Char] #-}
{-# SPECIALIZE neighbors :: MapGrid Int  -> (Int, Int) -> [Int]  #-}
{-# SPECIALIZE neighbors :: MapGrid Bool -> (Int, Int) -> [Bool] #-}
{-# SPECIALIZE neighbors :: UArrayGrid Char -> (Int, Int) -> [Char] #-}
{-# SPECIALIZE neighbors :: UArrayGrid Int  -> (Int, Int) -> [Int]  #-}
{-# SPECIALIZE neighbors :: UArrayGrid Bool -> (Int, Int) -> [Bool] #-}

ineighbors :: Grid g => g -> (Int, Int) -> [((Int, Int), Cell g)]
ineighbors g pos = mapMaybe sequence $ ineighbors' g pos

hvneighbors :: Grid g => g -> (Int, Int) -> [Cell g]
hvneighbors g pos = mapMaybe snd $ ihvneighbors' g pos
{-# SPECIALIZE hvneighbors :: MapGrid Char -> (Int, Int) -> [Char] #-}
{-# SPECIALIZE hvneighbors :: MapGrid Int  -> (Int, Int) -> [Int]  #-}
{-# SPECIALIZE hvneighbors :: MapGrid Bool -> (Int, Int) -> [Bool] #-}
{-# SPECIALIZE hvneighbors :: UArrayGrid Char -> (Int, Int) -> [Char] #-}
{-# SPECIALIZE hvneighbors :: UArrayGrid Int  -> (Int, Int) -> [Int]  #-}
{-# SPECIALIZE hvneighbors :: UArrayGrid Bool -> (Int, Int) -> [Bool] #-}

ihvneighbors :: Grid g => g -> (Int, Int) -> [((Int, Int), Cell g)]
ihvneighbors g pos = mapMaybe sequence $ ihvneighbors' g pos

ineighbors' :: Grid g => g -> (Int, Int) -> [((Int, Int), Maybe (Cell g))]
ineighbors' g (x, y) = map (\p -> (p, g `lookup` p)) [ (x - 1, y - 1)
                                                     , (x - 1, y    )
                                                     , (x - 1, y + 1)
                                                     , (x   , y - 1)
                                                     , (x   , y + 1)
                                                     , (x + 1, y - 1)
                                                     , (x + 1, y    )
                                                     , (x + 1, y + 1)
                                                     ]

ihvneighbors' :: Grid g => g -> (Int, Int) -> [((Int, Int), Maybe (Cell g))]
ihvneighbors' g (x, y) = map (\p -> (p, g `lookup` p)) [ (x - 1, y    )
                                                       , (x    , y - 1)
                                                       , (x + 1, y    )
                                                       , (x    , y + 1)
                                                       ]

iray :: Grid g => g -> V2 Int -> (Int, Int) -> [((Int, Int), Cell g)]
iray g d pos =
  catMaybes
  . takeWhile isJust
  . map (\(V2 p) -> (p,) <$> lookup g p)
  $ map (\i -> fromInteger i * d + v2 pos) [0..]
{-# SPECIALIZE iray :: UArrayGrid Char -> V2 Int -> (Int, Int) -> [((Int, Int), Char)] #-}
{-# SPECIALIZE iray :: UArrayGrid Int  -> V2 Int -> (Int, Int) -> [((Int, Int), Int)] #-}
{-# SPECIALIZE iray :: UArrayGrid Bool -> V2 Int -> (Int, Int) -> [((Int, Int), Bool)] #-}
{-# SPECIALIZE iray :: MapGrid Char -> V2 Int -> (Int, Int) -> [((Int, Int), Char)] #-}
{-# SPECIALIZE iray :: MapGrid Int  -> V2 Int -> (Int, Int) -> [((Int, Int), Int)] #-}
{-# SPECIALIZE iray :: MapGrid Bool -> V2 Int -> (Int, Int) -> [((Int, Int), Bool)] #-}


type MapGrid = HashMap (Int, Int)

instance Grid (HashMap (Int, Int) a) where
  {-# SPECIALIZE instance Grid (MapGrid Char) #-}
  {-# SPECIALIZE instance Grid (MapGrid Int) #-}
  {-# SPECIALIZE instance Grid (MapGrid Bool) #-}
  type Cell (MapGrid a) = a
  toLists = fromMapGrid
  lookup = (HashMap.!?)
  parse = parseMapGrid
  toList = HashMap.toList

type ArrayGrid = Array (Int, Int)
type UArrayGrid = UArray (Int, Int)

instance IArray UArray e => Grid (UArrayGrid e) where
  {-# SPECIALIZE instance Grid (UArrayGrid Char) #-}
  {-# SPECIALIZE instance Grid (UArrayGrid Int) #-}
  {-# SPECIALIZE instance Grid (UArrayGrid Word8) #-}
  {-# SPECIALIZE instance Grid (UArrayGrid Bool) #-}
  type Cell (UArrayGrid e) = e
  toLists g =
    let (_, (w, h)) = UA.bounds g
    in [[g UA.! (x, y) | x <- [0..w]] | y <- [0..h]]

  lookup = (UA.!?)
  parse = parseUArrayGrid
  toList = UA.assocs

instance Grid (ArrayGrid e) where
  type Cell (ArrayGrid e) = e
  toLists g =
    let (_, (w, h)) = A.bounds g
    in [[g A.! (x, y) | x <- [0..w]] | y <- [0..h]]

  lookup = (A.!?)
  parse = parseArrayGrid
  toList = A.assocs


readGrid :: Read a => String -> [[a]]
readGrid = parseGrid (read . pure)

readMapGrid :: Read a => String -> MapGrid a
readMapGrid = toMapGrid . readGrid

showGrid :: Show a => [[a]] -> String
showGrid = ppGrid (head . show)

showMapGrid :: Show a => MapGrid a -> String
showMapGrid = showGrid . fromMapGrid

parseGrid :: (Char -> a) -> String -> [[a]]
parseGrid parser = map (map parser) . lines

parseMapGrid :: (Char -> a) -> String -> MapGrid a
parseMapGrid parser =
  toMapGrid
  . parseGrid parser

parseArrayGrid :: (Char -> a) -> String -> ArrayGrid a
parseArrayGrid p input =
    let lg = map (map p) $ lines input
        w = maximum $ map length lg
        h = length lg
    in A.listArray ((0, 0), (w - 1, h - 1)) (concat $ transpose lg)

parseUArrayGrid :: IArray UArray a => (Char -> a) -> String -> UArrayGrid a
parseUArrayGrid p input =
    let lg = map (map p) $ lines input
        w = maximum $ map length lg
        h = length lg
    in UA.listArray ((0, 0), (w - 1, h - 1)) (concat $ transpose lg)

toMapGrid :: [[a]] -> HashMap (Int, Int) a
toMapGrid xs =
  HashMap.fromList [((ci, ri), cell) | (ri, row) <- zip [0..] xs
                                     , (ci, cell) <- zip [0..] row]

toArrayGrid :: [[a]] -> ArrayGrid a
toArrayGrid xs =
  let h = length xs
      w = maximum (map length xs)
  in UA.array ((0, 0), (w - 1, h - 1))
              [((ci, ri), cell) | (ri, row) <- zip [0..] xs
                                , (ci, cell) <- zip [0..] row
                                ]

toUArrayGrid :: IArray UArray a => [[a]] -> UArrayGrid a
toUArrayGrid xs =
  let h = length xs
      w = maximum (map length xs)
  in UA.array ((0, 0), (w - 1, h - 1))
              [((ci, ri), cell) | (ri, row) <- zip [0..] xs
                                , (ci, cell) <- zip [0..] row
                                ]

ppGrid :: (a -> Char) -> [[a]] -> String
ppGrid pp = unlines . map (map pp)

ppMapGrid :: (a -> Char) -> MapGrid a -> String
ppMapGrid pp = ppGrid pp . fromMapGrid

fromMapGrid :: MapGrid a -> [[a]]
fromMapGrid g =
  let (cs, rs) = unzip $ HashMap.keys g
      cm = maximum cs
      rm = maximum rs
  in fromMapGrid' (cm + 1) (rm + 1) g

fromMapGrid' :: Int -> Int -> MapGrid a -> [[a]]
fromMapGrid' w h g =
  map (map (g HashMap.!))
  $ [[(ci, ri) | ci <- [0..w-1]]  | ri <- [0..h-1]]
