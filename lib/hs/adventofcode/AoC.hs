{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module AoC ( tce
           , fixpoint

           -- vectors
           , V2(V2)
           , V3(V3)
           , v2
           , v3
           , dropX
           , dropY
           , dropZ
           , extendX
           , extendY
           , extendZ
           , extendX'
           , extendY'
           , extendZ'

           -- number theory
           , gcdExt
           , modInv

           -- statistics
           , mean
           , median

           -- binary
           , bitsFromBools
           , boolsFromBits
           , readBinary

           -- counting
           , Counter
           , counter
           , mostCommon
           , leastCommon
           , module AoC.Grid ) where


import Control.Applicative (liftA2)
import Data.Bits (Bits, setBit, testBit)
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (maximumBy, minimumBy, sort)
import Data.Ord (comparing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified AoC.Grid
import Debug.Trace (trace)

tce :: Show a => String -> a -> a
tce m v = trace (m ++ ": " ++ show v) v

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a =
  let xs = iterate f a
  in fst . head . filter (uncurry (==)) $ zip (drop 1 xs) xs


newtype V2 a = V2 (a, a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

v2 = V2

instance Applicative V2 where
  pure x = V2 (x, x)
  V2 (f1, f2) <*> V2 (x1, x2) = V2 (f1 x1, f2 x2)

instance Num a => Num (V2 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

dropX (V3 (_, y, z)) = v2 (y, z)
dropY (V3 (x, _, z)) = v2 (x, z)
dropZ (V3 (x, y, _)) = v2 (x, y)

extendX = extendX' 0
extendY = extendY' 0
extendZ = extendZ' 0

extendX' x (V2 (y, z)) = V3 (x, y, z)
extendY' y (V2 (x, z)) = V3 (x, y, z)
extendZ' z (V2 (x, y)) = V3 (x, y, z)

newtype V3 a = V3 (a, a, a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

v3 = V3

instance Applicative V3 where
  pure x = V3 (x, x, x)
  V3 (f1, f2, f3) <*> V3 (x1, x2, x3) = V3 (f1 x1, f2 x2, f3 x3)

instance Num a => Num (V3 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

median :: (Foldable t, Ord a, Fractional a) => t a -> a
median xs =
  case (length xs `divMod` 2, sort (toList xs)) of
    (_, []) -> error "median of empty list"
    ((n, 0), s) -> s !! n
    ((n, _), s) -> sum (take 2 . drop n $ s) / 2

mean :: (Foldable t, Eq a, Fractional a) => t a -> a
mean xs =
  case foldr f (0, 0) xs of
    (_, 0) -> error "mean of empty list"
    (l, s) -> s / l
  where f x (!l, !s) = (l + 1, s + x)

modInv :: Int -> Int -> Maybe Int
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

gcdExt :: Int -> Int -> (Int, Int, Int)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)


readBinary :: (Num a, Bits a) => String -> a
readBinary = bitsFromBools . map (== '1')

bitsFromBools :: (Num b, Bits b) => [Bool] -> b
bitsFromBools = foldl f 0 . zip [0..] . reverse
  where f acc (i, True) = setBit acc i
        f acc _ = acc

boolsFromBits n b = reverse $ map (testBit b) [0..n-1]

type Counter a = Map a Int

counter :: (Ord a, Foldable t) => t a -> Counter a
counter = Map.fromListWith (+) . flip zip (repeat 1) . toList

mostCommon :: (Ord a, Foldable t) => t a -> a
mostCommon = fst . maximumBy (comparing snd) . Map.toList . counter

leastCommon :: (Ord a, Foldable t) => t a -> a
leastCommon = fst . minimumBy (comparing snd) . Map.toList . counter
