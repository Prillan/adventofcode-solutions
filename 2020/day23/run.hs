{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Foldable (toList)
import Data.Semigroup ((<>))
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq(Empty, (:<|), (:|>)))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Text.Read (readMaybe)

parseAll = Seq.fromList
           . mapMaybe (readMaybe @Cup . pure)

type Cup = Int
type Cups = Seq Cup

cupMin :: Cup
cupMin = 1

insertAfter :: Cups -> Cup -> Cups -> Cups
insertAfter els dest xs =
  case Seq.breakl (== dest) xs of
    (before, _ :<| after) -> before <> (dest :<| els) <> after
    (before, Empty)       -> error "asdf"

insertAfter' :: Cups -> Cup -> Cups -> Maybe Cups
insertAfter' els dest xs =
  case Seq.breakl (== dest) xs of
    (before, _ :<| after) -> Just $ before <> (dest :<| els) <> after
    (before, Empty)       -> Nothing


rotateTo :: Cups -> Cup -> Cups
rotateTo xs t =
  Seq.take (Seq.length xs)
  $ Seq.dropWhileL (/= t) (xs <> xs)

move :: Cup -> Cups -> Cups
move cupMax (curr :<| c1 :<| c2 :<| c3 :<| rest) =
  let dests = filter (\x -> x /= c1 && x /= c2 && x /= c3)
              . drop 1
              $ iterate (\case 1 -> cupMax
                               x -> x - 1) curr
      dest = head dests
      result = insertAfter [c1, c2, c3] dest (rest :|> curr)
  in result

part1 input =
  let final = iterate (move (maximum input)) input !! 100
      final' = iterate (move2 (maximum input)) (asIntMap $ toList input) !! 100

      result = concatMap show
               . Seq.drop 1
               $ final `rotateTo` 1
      result' = concatMap show
                . Seq.drop 1
                . flip rotateTo 1
                $ Seq.fromList (fromCupsV2 final')
  in (result, result')

data CupsV2 = CupsV2 { cups :: !(IntMap Cup)
                     , currentCup :: !Cup
                     , lastCup :: !Cup }
  deriving Show

move2 :: Cup -> CupsV2 -> CupsV2
move2 cupMax CupsV2 {..} =
  let _:c1:c2:c3:_ = iterate (cups IntMap.!) currentCup
      dests = filter (\x -> x /= c1 && x /= c2 && x /= c3)
              . drop 1
              $ iterate (\case 1 -> cupMax
                               x -> x - 1) currentCup
      dest = head dests
      afterDest = cups IntMap.! dest
      afterC3 = cups IntMap.! c3
      -- curr
      -- -> c1
      -- -> c2
      -- -> c3
      -- -> afterC3
      -- -> ...
      -- -> dest
      -- -> afterDest
      -- -> ...
      -- -> last

      -- afterC3
      -- -> ...
      -- -> dest *
      -- -> c1
      -- -> c2
      -- -> c3 *
      -- -> afterDest
      -- -> ...
      -- -> last *
      -- -> curr *
      toInsert
        | dest == lastCup =
            IntMap.fromList [ (dest, c1)
                            , (c3, currentCup)
                            , (currentCup, afterC3) ]
        | otherwise =
            IntMap.fromList [ (dest, c1)
                            , (c3, afterDest)
                            , (lastCup, currentCup)
                            , (currentCup, afterC3) ]
      last' = currentCup
      curr' = afterC3
      result = IntMap.union toInsert cups
      cs = CupsV2 result curr' last'
  in cs

asIntMap :: [Cup] -> CupsV2
asIntMap xs =
  let l = last xs
      c = head xs
      cs = IntMap.fromList $ (last xs, head xs):zip xs (drop 1 xs)
  in CupsV2 cs c l

fromCupsV2 :: CupsV2 -> [Cup]
fromCupsV2 CupsV2 {..} =
  let cs = iterate' (cups IntMap.!) currentCup
  in currentCup:takeWhile (/= currentCup) (drop 1 cs)

part2 input =
  let end = 1_000_000
      start = 10
      initial = input <> Seq.iterateN (1 + end - start) (+1) start
      CupsV2 { cups = final}
        = iterate' (move2 end) (asIntMap $ toList initial) !! 10_000_000
  in product $ take 3 (iterate (final IntMap.!) 1)

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
