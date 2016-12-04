{-# LANGUAGE OverloadedLists #-}
import           Data.List (intercalate)
import           Data.Maybe (mapMaybe)
import           Data.Vector ((!), (!?))
import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Bool)

printGrid = intercalate "\n" . map (map (\x -> if x then '#' else '.')) . map V.toList . V.toList

readGrid :: String -> Grid
readGrid = V.fromList . map (V.fromList . line) . lines
  where line = map (\x -> case x of
                            '#' -> True
                            '.' -> False
                   )

neighbours :: Grid -> Int -> Int -> [Bool]
neighbours grid x y = mapMaybe (\(x', y') -> (grid !? y') >>= (!? x'))
                               [(x', y') | x' <- [x-1, x, x+1]
                                         , y' <- [y-1, y, y+1]
                                         , (x', y') /= (x, y)]

step :: Grid -> Grid
step grid = V.map (\y -> V.map (\x -> f x y) [0..s-1]) [0..s-1]
  where s = V.length grid
        f x y = case ((grid ! y) ! x, length (filter id (neighbours grid x y))) of
                  (True, 2) -> True
                  (True, 3) -> True
                  (False, 3) -> True
                  _ -> False

turnOnCorners grid = V.map (\y -> V.map (\x -> f x y) [0..s-1]) [0..s-1]
  where s = V.length grid
        corners = [(0, 0), (0, s - 1), (s - 1, 0), (s - 1, s - 1)] :: [(Int, Int)]
        f x y | elem (x, y) corners = True
              | otherwise = (grid ! y) ! x

step' :: Grid -> Grid
step' grid = V.map (\y -> V.map (\x -> f x y) [0..s-1]) [0..s-1]
  where s = V.length grid
        corners = [(0, 0), (0, s - 1), (s - 1, 0), (s - 1, s - 1)] :: [(Int, Int)]
        f x y
          | elem (x, y) corners = True
          | otherwise = case ((grid ! y) ! x, length (filter id (neighbours grid x y))) of
                              (True, 2) -> True
                              (True, 3) -> True
                              (False, 3) -> True
                              _ -> False

on :: Grid -> Int
on = V.sum . V.map (V.length . V.filter id)

part1 g = on $ (iterate step g) !! 100
part2 g = on $ (iterate step' g') !! 100
  where g' = turnOnCorners g

main = do
   input <- readGrid <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
