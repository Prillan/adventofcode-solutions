{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
import AoC.Grid (readGrid)

import Data.List (tails, transpose, union)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap


-- Might be nicer to be a bit more imperative.
-- Use Vector for speed.

parseAll :: String -> [[Int]]
parseAll = readGrid @Int

visible :: Int -> [Int] -> [Int]
visible start = go start 0
  where go top i =
          \case [] -> []
                x:xs
                  | x > top  -> i:go x (i + 1) xs
                  | otherwise -> go top (i + 1) xs

visible' :: [Int] -> [Int]
visible' = map (\(y:ys) -> go y 0 ys) . filter (not . null) . tails
  where go start count =
          \case [] -> count
                x:xs
                  | x >= start -> count + 1
                  | otherwise  -> go start (count + 1) xs

visibleH :: Int -> [Int] -> [Int]
visibleH start xs =
  let l  = length xs
      v1 = visible start xs
      v2 = map ((l - 1) -) (visible start (reverse xs))
  in v1 `union` v2

visibleH' :: [Int] -> [Int]
visibleH' xs =
  let v1 = visible' xs
      v2 = reverse (visible' (reverse xs))
  in zipWith (*) v1 v2

allVisible :: [[Int]] -> [(Int, Int)]
allVisible grid =
  let rows =
        concat [map (i,) (visibleH (-1) r)
               | (i, r) <- zip [0..] grid ]
      cols =
        concat [map (,i) (visibleH (-1) c)
               | (i, c) <- zip [0..] (transpose grid) ]
  in
    rows `union` cols

scores :: [[Int]] -> HashMap (Int, Int) Int
scores grid =
  let rows =
        [((ri, ci), cnt)
        | (ri, r)   <- zip [0..] grid
        , (ci, cnt) <- zip [0..] (visibleH' r)]
      cols =
        [((ri, ci), cnt)
        | (ci, c)   <- zip [0..] (transpose grid)
        , (ri, cnt) <- zip [0..] (visibleH' c)]
  in HashMap.fromListWith (*) (rows ++ cols)

part1 :: [[Int]] -> Int
part1 = length . allVisible

part2 :: [[Int]] -> Int
part2 = maximum . HashMap.elems . scores

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
