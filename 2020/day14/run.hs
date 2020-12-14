{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Control.Applicative ((<|>))
import Data.Bits
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

-- Part 1

data Mask = Mask { mask0 :: Integer
                 , mask1 :: Integer }
  deriving Show
type State = (Mask, Map Int Integer)

initial = (Mask 0 0, Map.empty)

parse :: String -> Either Mask (Int, Integer)
parse x = fromJust $ (Left <$> parseMask x) <|> (Right <$> parseMem x)

parseMask x = do
  bits <- stripPrefix "mask = " x
  let m0 = map not $ map (== '0') bits
      m1 = map (== '1') bits
  pure $ Mask (bitsFromBools m0) (bitsFromBools m1)

parseMem x = listToMaybe $ do
  (addr, rest) <- reads $ drop 4 x
  (val, _) <- reads $ drop 4 rest
  pure (addr, val)

applyMask Mask {..} v = (v .&. mask0) .|. mask1

parseAll p =
  map p . lines

step (_, vals) (Left m) = (m, vals)
step (m, vals) (Right (i, v)) = (m, Map.insert i (applyMask m v) vals)

part1 = sum . Map.elems . snd . foldl' step initial

-- Part 2

newtype Mask2 = Mask2 [Integer -> Int -> Integer]

parse2 :: String -> Either [Mask2] (Integer, Integer)
parse2 x = fromJust $ (Left <$> parseMask2 x) <|> (Right <$> parseMem x)

parseMask2 x =
  map (Mask2 . reverse) . traverse f <$> stripPrefix "mask = " x
  where f '0' = [const]
        f '1' = [setBit]
        f 'X' = [clearBit, setBit]

applyMask2 :: Mask2 -> Integer -> Integer
applyMask2 (Mask2 fs) v =
  foldl' (\v' (i, f) -> f v' i) v $ zip [0 :: Int ..] fs

step2 (_, vals) (Left m) = (m, vals)
step2 (ms, vals) (Right (i, v)) =
  let !vals' =
        flip Map.union vals -- flip needed to prefer new values
        . Map.fromList
        . map (,v)
        . map (flip applyMask2 i)
        $ ms
  in
    (ms, vals')

part2 = sum . Map.elems . snd . foldl' step2 ([], Map.empty)

main = do
   input <- readFile "input.txt"
   print $ part1 (parseAll parse input)
   print $ part2 (parseAll parse2 input)
