module AoC.Grid where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

type MapGrid = HashMap (Int, Int)

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

toMapGrid :: [[a]] -> HashMap (Int, Int) a
toMapGrid xs =
  HashMap.fromList [((ci, ri), cell) | (ri, row) <- zip [0..] xs
                                 , (ci, cell) <- zip [0..] row]

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
