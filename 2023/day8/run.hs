{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

-- TODO: Cleanup

import Data.Char
import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Text.ParserCombinators.ReadP

type N = Int

run p = fmap fst . listToMaybe . readP_to_S p

parse = run do
  f <- munch1 isAsciiUpper
  _ <- string " = ("
  l <- munch1 isAsciiUpper
  _ <- string ", "
  r <- munch1 isAsciiUpper
  pure (f, (l, r))

parseAll input =
  let [seq, network] = splitOn "\n\n" input
  in
    ( seq
    , HashMap.fromList . map (fromJust . parse) $ lines network
    )

follow s network start p = go 0 start (cycle s)
  where go !i !x ds
          | p x = i
          | otherwise =
            let (n, ds') = follow1 ds network x
            in
              go (i + 1) n ds'

follow1 (d:ds) network x =
  let (!l, !r) = network HashMap.! x
  in case d of
       'R' -> (r, ds)
       'L' -> (l, ds)

snd3 (_, y, _) = y

part1 (s, network) =
  follow s network "AAA" (== "ZZZ")

part2 (s, network) =
  let starts = filter ("A" `isSuffixOf`) $ HashMap.keys network
      solve start =
        follow s network start ("Z" `isSuffixOf`)
  in
    foldl lcm 1 $ map solve (sort starts)

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
