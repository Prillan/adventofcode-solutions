{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import GHC.Ix (unsafeIndex)

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

type N = Int

data Cucumber = East | South
  deriving (Show, Eq, Ord, Enum, Bounded)

data Cell = Empty | Filled !Cucumber
  deriving (Show, Eq, Ord)

type Grid = Vector Cell

parseAll :: String -> ((N, N), Grid)
parseAll input =
  let ls = lines input
      xm = maximum $ map length ls
      ym = length ls
      v = V.fromList $
          [ f cell | row  <- ls
                   , cell <- row
                   ]
      b = (ym - 1, xm - 1)
  in (b, v)
  where f = \case 'v' -> Filled South
                  '>' -> Filled East
                  '.' -> Empty

ix :: (N, N) -> (N, N) -> Int
ix b = unsafeIndex ((0, 0), b)
{-# INLINE ix #-}

unIx :: (N, N) -> Int -> (N, N)
unIx (_, xm) i = i `divMod` (xm + 1)
{-# INLINE unIx #-}

stepEast :: (N, N) -> Grid -> Grid
stepEast b@(_, xm) g = V.modify action g
  where action v = V.iforM_ g \i current ->
          let (y, x)   = unIx b i
              righti   = ix b (y, (x + 1) `mod` (xm + 1))
              currenti = ix b (y, x)
          in
            case (current, g V.! righti) of
              (Filled East, Empty) ->
                VM.write v currenti Empty *> VM.write v righti (Filled East)
              _ -> pure ()
        {-# INLINE action #-}

stepSouth :: (N, N) -> Grid -> Grid
stepSouth b@(ym, _) g = V.modify action g
  where action v = V.iforM_ g \i current ->
          let (y, x)   = unIx b i
              downi    = ix b ((y + 1) `mod` (ym + 1), x)
              currenti = ix b (y, x)
          in
            case (current, g V.! downi) of
              (Filled South, Empty) ->
                VM.write v currenti Empty *> VM.write v downi (Filled South)
              _ -> pure ()
        {-# INLINE action #-}

step :: (N, N) -> Grid -> Grid
step b = stepSouth b . stepEast b
{-# INLINE step #-}

asList :: (N, N) -> Grid -> [[Cell]]
asList b@(ym, xm) g =
  [ [ g V.! ix b (y, x) | x <- [0..xm] ] | y <- [0..ym] ]


pp :: (N, N) -> Grid -> String
pp b = ppGrid f . asList b
  where f = \case Filled East  -> '>'
                  Filled South -> 'v'
                  Empty        -> '.'


part1 :: ((N, N), Grid) -> Int
part1 (b, g) = fst $ fixpointi (step b) g

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
