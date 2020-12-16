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
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
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

for = flip map

-- Sets things up by
--   - checking which fields are valid for each column
--   - replacing each field with an integer for more speed
--   - sorting the input by number of candidates (smaller first)
--     (returns the permutation in order to unsort it later)
prep :: [Field] -> [[Int]] -> ([Int], [([Int], [Int])])
prep fs fnums =
  let fieldMap = Map.fromList $ zip fs [0..]
      validFields =
        for fnums $ \nums ->
          map (fieldMap Map.!)
          . filter (\f -> all (`validForField` f) nums)
          $ fs
      (perm, sortedFields, sortedNums) =
        unzip3
        . sortOn (\(_, vfs, _) -> length vfs)
        $ zip3 [0..] validFields fnums
  in (perm, zip sortedFields sortedNums)

assignFields fs tickets =
  let (perm, xs) = prep fs $ transpose tickets
      results = assignFields' xs
      fixResult r =
        map snd         -- -+
        . sortOn fst    --  +--- unsort the fields
        . zip perm      -- -+
        $ map (fs !!) r -- look up the field by index
  in map fixResult results

assignFields' :: [([Int], [Int])] -> [[Int]]
assignFields' = go IntSet.empty
  where go assigned [] = [[]]
        go assigned ((fs, nums):rest) = do
          o <- filter (not . flip IntSet.member assigned)
               $ fs
          map (o:) (go (IntSet.insert o assigned) rest)

part2 (fs, ticket, others) =
  let valid  = filter (all (`validValue` fs)) others
      fields = head $ assignFields fs valid
  in product
     . map snd
     . filter (isPrefixOf "departure" . fieldName . fst)
     $ zip fields ticket

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
