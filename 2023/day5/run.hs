{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import Data.Function ((&))
import Data.Bifunctor (bimap)
import Data.Foldable ()
import Data.List (foldl', sort)
import Data.List.Split (chunksOf, splitOn)

type N = Int

type Mapping = [(N, N, N)]
type Input = ([N], [Mapping])

parseNums :: String -> [N]
parseNums = map (read @N) . words

parseMap :: [String] -> [(N, N, N)]
parseMap = map f . drop 1
  where f m = parseNums m & \[x, y, z] -> (x, y, z)

parseAll :: String -> Input
parseAll input =
  let ([seeds]:maps) = splitOn [""] $ lines input
  in ( parseNums $ dropWhile (/= ' ') seeds
     , map parseMap maps
     )

applyMap :: N -> Mapping -> N
applyMap v = maybe v id . foldl' f Nothing
  where f (Just v') _ = Just v'
        f _ (d, s, l)
          | s <= v && v < s + l = Just $ v - s + d
          | otherwise           = Nothing


part1 :: Input -> N
part1 (seeds, maps) =
  minimum $ map (\s -> foldl' applyMap s maps) seeds

data Interval = Interval { istart  :: N
                         , ilength :: N
                         }
  deriving Show

fromEndpoints :: N -> N -> Interval
fromEndpoints s e = Interval s (e - s)

iempty :: Interval -> Bool
iempty = (== 0) . ilength

translate :: N -> Interval -> Interval
translate n i = Interval (n + istart i) (ilength i)

iend :: Interval -> N
iend i = istart i + ilength i

intersect :: Interval -> Interval -> ([Interval], [Interval])
intersect i1 i2 =
  let s1 = istart i1
      s2 = istart i2
      e1 = iend i1
      e2 = iend i2
  in case sort [s1,s2,e1,e2] of
       xs | take 2 xs == [s1, e1]         -> ([], [i1])
          | take 2 xs == [s2, e2]         -> ([], [i1])
          |        xs == [s1, s2, e1, e2] -> ([fromEndpoints s2 e1], [fromEndpoints s1 s2])
          |        xs == [s1, s2, e2, e1] -> ([fromEndpoints s2 e2], [fromEndpoints s1 s2, fromEndpoints e2 e1])
          |        xs == [s2, s1, e2, e1] -> ([fromEndpoints s1 e2], [fromEndpoints e2 e1])
          |        xs == [s2, s1, e1, e2] -> ([fromEndpoints s1 e1], [])


asIntervals :: Mapping -> [(Interval, N)]
asIntervals = map (\(d, s, l) -> (Interval s l, d - s))

applyIMap :: [(Interval, N)] -> Interval -> [Interval]
applyIMap maps i = go maps [i] []
  where go :: [(Interval, N)] -> [Interval] -> [Interval] -> [Interval]
        go     [] rem is = rem ++ is
        go ((m, diff):ms) rem is =
          let (is', rem') = bimap clean clean . unzip $ map (`Main.intersect` m) rem
          in go ms rem' (is ++ map (translate diff) is')

        clean = filter (not . iempty ) . concat

part2 :: Input -> N
part2 (seeds, maps) = minimum . map istart $ go seeds' maps'
  where seeds' = map (\[s, l] -> Interval s l) $ chunksOf 2 seeds
        maps'  = map asIntervals maps
        go xs = \case []   -> xs
                      m:ms -> go (concatMap (applyIMap m) xs) ms

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
