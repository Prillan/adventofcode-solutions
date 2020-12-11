module AoC ( tce
           , fixpoint
           , module AoC.Grid ) where

import qualified AoC.Grid
import Debug.Trace (trace)

tce :: Show a => String -> a -> a
tce m v = trace (m ++ ": " ++ show v) v

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a =
  let xs = iterate f a
  in fst . head . filter (uncurry (==)) $ zip (drop 1 xs) xs
