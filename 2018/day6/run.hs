{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Counter a = Map a Int

counter :: Ord a => [a] -> Counter a
counter = Map.fromListWith (+) . flip zip (repeat 1)

maxCount :: Counter a -> Int
maxCount = maximum . map snd . Map.toList

parseAll :: String -> [Point]
parseAll = map read
           . map (\s -> "(" ++ s ++ ")")
           . lines

data Bounds = Bounds { left, right, top, bottom :: Int }
  deriving Show
type Point = (Int, Int)


bounds :: [Point] -> Bounds
bounds points = Bounds { top    = minimum . map snd $ points
                       , bottom = maximum . map snd $ points
                       , right  = maximum . map fst $ points
                       , left   = minimum . map fst $ points }

boundary :: Bounds -> [Point]
boundary Bounds {..} =
  concat [ map (,top)    [left..right]
         , map (,bottom) [left..right]
         , map (left,)   [top..bottom]
         , map (right,)  [top..bottom] ]

fill :: Bounds -> [Point]
fill Bounds {..} = [(x, y) | x <- [left..right], y <- [top..bottom]]

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

closest :: [Point] -> Point -> [(Point, Int)]
closest points p =
  sortBy (comparing snd) . map (\p' -> (p', distance p p')) $ points

removeInfinite :: Bounds -> [Point] -> [Point]
removeInfinite b points =
  let inf = map (fst . head . closest points) (boundary b)
  in
    filter (not . flip elem inf) points

ambiguous :: [(Point, Int)] -> Bool
ambiguous ((_, c1):(_, c2):_) = c1 == c2
ambiguous _ = False

countClosest :: [Point] -> [Point] -> Counter Point
countClosest target fixed =
  counter
  . map (fst . head)
  . filter (not . ambiguous)
  . map (closest fixed)
  $ target

part1 :: [Point] -> Int
part1 points =
  let b = bounds points
      valid = removeInfinite b points
  in
    maxCount
    . Map.filterWithKey (\k _ -> k `elem` valid)
    . countClosest (fill b)
    $ points

totalDistance :: [Point] -> Point  -> Int
totalDistance points p = sum (map (distance p) points)

part2 :: Int -> [Point] -> Int
part2 m points =
  let b = bounds points
  in
    length . filter (< m) . map (totalDistance points) $ fill b

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 10000 input)
