{-# LANGUAGE BangPatterns #-}
import Data.Bits (xor)
import Data.Bool (bool)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

dragon :: [Bool] -> [Bool]
dragon a =
  let b = map not $ reverse a
      c = a ++ [False] ++ b
  in c

iterated n = foldr (.) id (replicate n dragon)

bits :: [Bool] -> String
bits = map (bool '0' '1')

unbits :: String -> [Bool]
unbits = map f
  where f '0' = False
        f '1' = True

fill :: Int -> [Bool] -> [Bool]
fill len start =
  let n = ceiling (logBase 2 $ (fromIntegral (len + 1) / fromIntegral (length start + 1))) - 1
      a = iterated n start
      m = len - (2^n * (length start + 1))
      b = map not $ take m $ reverse a
  in a ++ [False] ++ b

checksum :: Int -> [Bool] -> [Bool]
checksum len bs
  | odd len   = bs
  | otherwise =
    let go (x:y:rest) = (not $ x `xor` y):(go rest)
        go [] = []
    in checksum (len `div` 2) (go bs)

solve len start = bits . checksum len $ fill len (unbits start)

part1 = solve 272 "01111010110010011"
part2 = solve 35651584 "01111010110010011"

main = do
  hSetBuffering stdout NoBuffering
  print part1
  print part2
