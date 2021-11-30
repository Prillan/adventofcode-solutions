import Data.Foldable
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

parseAll :: String -> [[Integer]]
parseAll =
  map (map read . splitOn "-") . lines

step vals n = foldr blacklist n vals
  where blacklist [low, high] min
          | low <= min && min <= high = high + 1
          | otherwise = min
part1 input =
  let vals = iterate (step input) 0
  in
    fst . head . filter (uncurry (==)) $ zip vals (tail vals)

tpl [x, y] = (x, y)

type Interval = (Integer, Integer)

--mergeAll :: [Interval] -> [Interval]
mergeAll ints =
  let s :: Either Integer Integer -> (Integer, Integer)
      s (Left x) = (x, 0)
      s (Right x) = (x, 1)

      points = sortOn s $ do
        (x, y) <- ints
        [Left x, Right y]

      step (Left x) (Nothing, acc) = (Just (1, x), acc)
      step (Left x) (Just (c, y), acc) = (Just (c+1, y), acc)
      step (Right x) (Just (1, y), acc) = (Nothing, (y, x):acc)
      step (Right x) (Just (c, y), acc) = (Just (c-1, y), acc)
      step x y = error $ show x ++ " | " ++ show y
  in snd $ foldl' (flip step) (Nothing, []) points

width :: Interval -> Integer
width (x, y) = 1 + y - x

count :: [Interval] -> Integer
count ints = 2 ^ 32 - sum (map width ints)

part2 = count . mergeAll . map tpl

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
