{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Data.List
import Data.List.Split

type N = Int
data RGB = RGB { red :: N
               , green :: N
               , blue :: N
               }
  deriving Show

instance Semigroup RGB where
  x <> y = RGB { red = max (red x) (red y)
               , green = max (green x) (green y)
               , blue = max (blue x) (blue y)
               }

instance Monoid RGB where
  mempty = RGB 0 0 0

type Draw = RGB
type Game = (Int, [Draw])

power :: RGB -> N
power RGB{..} = red * green * blue

parseDraw :: String -> Draw
parseDraw = foldl readCubes mempty . map words . splitOn ", "
  where readCubes x = \case
          [d, "red"] -> x { red = read d }
          [d, "green"] -> x { green = read d }
          [d, "blue"] -> x { blue = read d }

parseGame :: String -> Game
parseGame input =
  let [gs, ds] = splitOn ": " input
      Just g = read @N <$> stripPrefix "Game " gs
      d = map parseDraw $ splitOn "; " ds
  in (g, d)

parseAll :: String -> [Game]
parseAll = map parseGame  . lines

valid :: RGB -> RGB -> Bool
valid setup x =
  red x <= red setup &&
  green x <= green setup &&
  blue x <= blue setup

feasible :: RGB -> [Draw] -> Bool
feasible setup = all (valid setup)

part1 :: [Game] -> N
part1 =
  sum
  . map fst
  . filter (feasible start . snd)
  where start = RGB 12 13 14

part2 :: [Game] -> N
part2 =
  sum
  . map (power . mconcat . snd)

main :: IO ()
main = main' "input.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
