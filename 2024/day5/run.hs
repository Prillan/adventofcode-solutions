{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import Control.Monad (guard)

import Data.Functor.Contravariant (Comparison(..))
import Data.List (sortBy)
import Data.List.Split (splitOn)

type N = Int
type Input = (Comparison N, [[N]])


-- Very, very ineffective, but fun :)
parseComp :: String -> Comparison N
parseComp input =
  let [x, y] = map (read @N) $ splitOn "|" input
  in Comparison . curry $ \case
    (x', y') | x' == x && y' == y -> LT
             | x' == y && y' == x -> GT
             | otherwise          -> EQ


parseUpdate :: String -> [N]
parseUpdate input = read @[N] $ "[" <> input <> "]"

parseAll :: String -> Input
parseAll input =
  let [comps, xs] = splitOn [""] $ lines input
  in (foldMap parseComp comps, map parseUpdate xs)

middle :: [a] -> a
middle xs =
  let l = length xs
  in xs !! (l `div` 2)

part1 :: Input -> N
part1 (comp, updates) =
  sum
  . map middle
  . filter (\u -> sortBy (getComparison comp) u == u)
  $ updates

part2 :: Input -> N
part2 (comp, updates) = sum do
  u <- updates
  let u' = sortBy (getComparison comp) u
  guard $ u' /= u
  pure $ middle u'

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
