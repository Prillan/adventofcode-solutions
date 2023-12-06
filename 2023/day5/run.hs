{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC hiding (tce)
import qualified AoC
import AoC.Grid

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

type N = Integer

data Range = Range { start :: N
                   , len :: N
                   }
  deriving (Eq, Show)

tce :: Show a => String -> a -> a
--tce = AoC.tce
tce _ = id

-- exclusive
end :: Range -> N
end Range{..} = start + len

data Mapping = Mapping { source :: Range
                       , diff :: N
                       }
  deriving Show

single :: N -> Range
single n = Range n 1

rintersect :: Range -> Range -> ([Range], [Range]) -- (included, excluded)
rintersect a b
  -- [ ] ( )
  | end a <= start b = ([], [a])
  -- ( ) [ ]
  | end b <= start a = ([], [a])
  -- (  [  ]  )
  | start b <= start a && end a <= end b = ([a], [])
  -- [  (  )  ]
  | start a <= start b && end b <= end a = ( [ Range { start = start b
                                                     , len = len b
                                                     }
                                             ]
                                           , [ Range { start = start a
                                                     , len = start b - start a
                                                     }
                                             , Range { start = end b
                                                     , len = end a - end b
                                                     }
                                             ]
                                           )
  -- [ ( ] )
  | start a <= start b && end a <= end b = ( [ Range { start = start b
                                                     , len = end a - start b
                                                     }
                                             ]
                                           , [ Range { start = start a
                                                     , len = start b - start a
                                                     }
                                             ]
                                           )
  -- ( [  ) ]
  | start b <= start a && end b <= end a = ( [ Range { start = start a
                                                     , len = end b - start a
                                                     }
                                             ]
                                           , [ Range { start = end b
                                                     , len = end a - end b
                                                     }
                                             ]
                                           )
  | otherwise = error $ unlines ["shouldn't happen, I think?", show a, show b]


test :: IO ()
test = do
  let c str a = case a of
                  Nothing -> pure ()
                  Just e -> error $ str <> ": " <> e
      a === b | a == b = Nothing
              | otherwise = Just $ unlines ["a /= b", "a: " <> show a, "b: " <> show b]
      r0  = Range 0 10
      r5  = Range 5 10
      r10 = Range 10 10
      r2_5 = Range 2 5
  c "0" $ rintersect r0 r10 === ([], [r0])
  c "1" $ rintersect r0 r5  === ([Range 5 5], [Range 0 5])
  c "2" $ rintersect r5 r10 === ([Range 10 5], [Range 5 5])
  c "3" $ rintersect r2_5 r0 === ([r2_5], [])
  c "4" $ rintersect r2_5 r5 === ([Range 5 2], [Range 2 3])
        

parseSeeds =
  map (read @N) . drop 1 . words

parseMap =
  map parseRange . drop 1

parseRange rng =
  let [dest, source, len] = map (read @N) (words rng)
  in Mapping { source = Range { start = source
                              , len = len
                              }
             , diff = dest - source
             }

parseAll input =
  let [seeds]:maps = splitOn [""] $ lines input
  in (parseSeeds seeds, map parseMap maps)


mapThing :: [Mapping] -> Range -> [Range]
mapThing mappings v = go [v] mappings
  where go :: [Range] -> [Mapping] -> [Range]
        go vs     []     = vs
        go []     _      = []
        go (v:vs) (m:ms) =
          case tce "intersect" (rintersect v (source m)) of
            -- excluded from m, continue with the next mapping
            ([], _)   -> go (v:vs) ms
            -- overlap
            ([i], es) -> i { start = start i + diff m }:go (es ++ vs) mappings

solve seedRanges maps = foldl' f seedRanges maps
  where f !seeds' m = concatMap (mapThing m) (tce "seeds" seeds')

part1 (seeds, maps) =
  minimum . map start $ solve (map single seeds) maps
part2 (seeds, maps) =
  let ranges = map (\[s, l] -> Range s l) $ chunksOf 2 seeds
  in minimum . map start $ solve ranges maps

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
