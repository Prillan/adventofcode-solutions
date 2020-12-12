{-# LANGUAGE DeriveFunctor #-}
module AoC ( tce
           , fixpoint
           , V2(V2)
           , V3(V3)
           , v2
           , v3
           , extendX
           , extendY
           , extendZ
           , extendX'
           , extendY'
           , extendZ'
           , module AoC.Grid ) where

import Control.Applicative (liftA2)

import qualified AoC.Grid
import Debug.Trace (trace)

tce :: Show a => String -> a -> a
tce m v = trace (m ++ ": " ++ show v) v

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a =
  let xs = iterate f a
  in fst . head . filter (uncurry (==)) $ zip (drop 1 xs) xs


newtype V2 a = V2 (a, a)
  deriving (Eq, Show, Functor)

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

extendX = extendX' 0
extendY = extendY' 0
extendZ = extendZ' 0

extendX' x (V2 (y, z)) = V3 (x, y, z)
extendY' y (V2 (x, z)) = V3 (x, y, z)
extendZ' z (V2 (x, y)) = V3 (x, y, z)

newtype V3 a = V3 (a, a, a)
  deriving (Eq, Show, Functor)

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
