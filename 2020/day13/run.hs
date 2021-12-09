{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.List.Split (splitOn)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- type Parser = Parsec (ErrorItem Char) String

parseAll xs =
  let s:ls:_ = lines xs
      busses =
        map (read @Int)
        . map (\case
                  "x" -> "-1"
                  v -> v
              )
        $ splitOn "," ls
  in (read @Int s, busses)

nextDepIn s l = l - (s `mod` l)

part1 (s, busses) =
  let nextId = minimumBy (comparing (nextDepIn s))
               . filter (>= 0)
               $ busses
  in nextId * (nextDepIn s nextId)


-- Solution without using brute force :)
addDep start base offset v =
  let shift = start `mod` v
      offset' = offset + shift
      Just vinv = v `modInv` base
      n = offset' * vinv
      start' = v * n - offset'
      base' = lcm base v
  in ((start + start') `mod` base', base')

part2 (_, busses) =
  let (_, v0):withOffsets = filter ((>= 0) . snd) $ zip [0..] busses
  in fst $ foldl (\(s, b) (v, o) -> addDep s b v o) (0, v0) withOffsets

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
