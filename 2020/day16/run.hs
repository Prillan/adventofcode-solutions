{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Control.Applicative ((<|>))
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Read (readMaybe)

unsafeMapMaybe f = map g
  where g x = maybe (error $ "bad input: " ++ show x) id (f x)

parse l =
  let parsed = (Left <$> parseField l) <|> (Right <$> parseNums l)
  in
    case parsed of
      Just x -> x
      Nothing -> error $ "failed to parse: " ++ show l

data Field = Field { fieldName   :: String
                   , fieldRanges :: [Range] }
  deriving (Show, Eq, Ord)

type Range = (Int, Int)
readRange x =
  case splitOn "-" x of
    [l, h] -> (,) <$> readMaybe l <*> readMaybe h
    _ -> Nothing

parseField x = do
  [name, ranges] <- pure $ splitOn ": " x
  Field name <$> traverse readRange (splitOn " or " ranges)
parseNums = traverse (readMaybe @Int) . splitOn ","

parseAll input =
  let [fields, ticket, others] = splitOn [""] $ lines input
  in
    ( unsafeMapMaybe parseField fields
    , head $ mapMaybe parseNums ticket
    , unsafeMapMaybe parseNums (drop 1 others))

inRange :: Int -> Range -> Bool
inRange v (l, h) = l <= v && v <= h

validForField v (Field _ rs) = any (v `inRange`) rs

validValue v = any (v `validForField`)

part1 (fs, _, others) =
  sum
  . filter (not . flip validValue fs)
  $ concat others

assignFields fs = go Set.empty fs . transpose
  where go assigned _ [] = [[]]
        go assigned remaining (fnums:rest) = do
          o <- filter (\f -> all (`validForField` f) fnums) remaining
          map (o:) (go (Set.insert o assigned) (filter (/= o) remaining) rest)

part2 (fs, ticket, others) =
  let valid  = filter (all (`validValue` fs)) others
      fields = head $ assignFields fs valid
  in product
     . map snd
     . filter (isPrefixOf "departure" . fieldName . fst)
     $ zip fields ticket

-- Runs in ~3m 30s
-- ./run  207,18s user 0,23s system 99% cpu 3:27,52 total
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
