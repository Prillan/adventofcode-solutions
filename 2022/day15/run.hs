{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
import AoC

import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import qualified Text.ParserCombinators.ReadP as P

type N = Int
type Sensor = (V2 N, V2 N)
type Ball = (V2 N, N)

sensorP :: P.ReadP Sensor
sensorP = do
  _ <- P.string "Sensor at "
  s <- v2P
  _ <- P.string ": closest beacon is at "
  b <- v2P
  pure (s, b)

v2P :: P.ReadP (V2 N)
v2P = do
  _ <- P.string "x="
  x <- numP
  _ <- P.string ", y="
  y <- numP
  pure $ v2 (x, y)

numP :: Read n => P.ReadP n
numP = read <$> P.choice [ (:) <$> P.char '-' <*> P.munch1 isDigit
                         , P.munch1 isDigit
                         ]

parse :: String -> Sensor
parse =
  (\[(s, "")] -> s)
  . P.readP_to_S sensorP

parseAll :: String -> [Sensor]
parseAll = map parse  . lines

toBall :: Sensor -> Ball
toBall (s, b) =
  (s, sum $ abs $ s - b)

bounds :: [Ball] -> (N, N)
bounds balls =
  let xmin = minimum $ map (\(V2 (cx, _), r) -> cx - r) balls
      xmax = maximum $ map (\(V2 (cx, _), r) -> cx + r) balls
  in (xmin, xmax)

inside :: V2 N -> Ball -> Bool
inside p (center, r) =
  sum (abs (p - center)) <= r

part1 :: N -> [Sensor] -> Int
part1 row i =
  let beacons = map snd i
      balls = map toBall i
      (xmin, xmax) = bounds balls
  in
    length
    . filter (not . (`elem` beacons))
    . filter (\p -> any (p `inside`) balls)
    . map (\x -> v2 (x, row))
    $ [xmin..xmax]


type Interval = (N, N)

slice :: N -> Ball -> Maybe Interval
slice y (V2 (cx, cy), r) =
  let ydist = abs $ cy - y
      xmin = cx - r + ydist
      xmax = cx + r - ydist
  in
    if ydist > r
    then Nothing
    else Just (xmin, xmax)

merge :: [Interval] -> [(N, Int)]
merge =
  sortBy (comparing fst <> comparing (negate . snd))
  . concatMap (\(l, h) -> [(l, 1), (h, -1)])

steps :: [Interval] -> [(Interval, Int)]
steps = init . drop 1 . go (-1) 0 . merge
  where go p cv =
          \case
            [] -> []
            (i, v):rest
              | p == i    -> go p (v+cv) rest
              | otherwise -> ((p, i), cv):go i (v+cv) rest

onlyMissing :: [Interval] -> [N]
onlyMissing =
    map (\((_, p2), _) -> p2 - 1)
    . filter (\((p1, p2), v) -> v == 0 && p1 + 2 == p2)
    . steps

onRow :: N -> [Ball] -> [N]
onRow y balls =
  onlyMissing $ mapMaybe (slice y) balls

part2 :: N -> [Sensor] -> N
part2 factor i =
  let balls = map toBall i
  in
    head
    . map (\(y, x) -> x * factor + y)
    . concatMap (\y -> sequence (y, onRow y balls))
    $ [0..factor]

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 2000000 input)
   print (part2 4000000 input)
