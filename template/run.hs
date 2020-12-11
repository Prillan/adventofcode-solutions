{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bits (xor)
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
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- type Parser = Parsec (ErrorItem Char) String

type Counter a = Map a Int

counter :: Ord a => [a] -> Counter a
counter = Map.fromListWith (+) . flip zip (repeat 1)

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

parseAll =
  map unsafeRight .
  map (parse _ "") . lines

part1 = id
part2 = id

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
--   print (part2 input)
