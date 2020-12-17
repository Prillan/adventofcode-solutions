{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Word (Word32)

-- Using Word32 instead of Int (64-bit) makes it run 300ms faster by
-- only allocating half the memory.
type N = Word32

parseAll =
  map (read @Int) . splitOn ","

numbers :: N -> [Int] -> N
numbers final start = runST $ do
  -- Create a vector to act as a lookup table for all elements
  -- maxBound means that the value hasn't appeared yet, otherwise it
  -- says which turn it appeared on.
  vec <- VM.replicate (fromIntegral final) (maxBound @N)
  -- Initialize the vector with the starting list.
  forM_ (zip start [0..]) $ \(i, t) ->
    VM.write vec i t
  -- Run it!
  go vec 0 (fromIntegral $ length start)
  where -- We've reached the final step and should stop.
        go _ speak t
          | t == final - 1 = pure $ fromIntegral speak
        -- Otherwise, go for it another turn.
        go table toSpeak t = do
          -- Lookup the next value to speak in our table.
          val <- VM.read table toSpeak
          let next = if val == maxBound
                       then 0       -- -+-- this is according to the
                       else t - val -- -+   problem description
          -- Write our spoken value to the table
          VM.write table toSpeak t
          -- Next turn!
          go table (fromIntegral next) (t + 1)

solve :: N -> [Int] -> N
solve idx = numbers idx

part1 = solve 2020
part2 = solve 30000000

-- Runs in ~1.4s
-- ./run  1,43s user 0,06s system 99% cpu 1,494 total
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
