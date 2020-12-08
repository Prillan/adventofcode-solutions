import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Maybe
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

parseAll = lines

binary :: Int -> String -> Int
binary n = fst . foldl go (0, n)
  where go (l, h) v
          | v == 'F' || v == 'L' = (l, (l + h) `div` 2)
          | otherwise            = ((l + h + 1) `div` 2, h)

findSeat :: String -> (Int, Int)
findSeat spec =
  let (rspec, cspec) = splitAt 7 spec
  in (binary 127 rspec, binary 7 cspec)

seatId :: (Int, Int) -> Int
seatId (r, c) = r * 8 + c

posFromId :: Int -> (Int, Int)
posFromId = (`divMod` 8)

part1 :: [String] -> Int
part1 = maximum
  . map seatId
  . map findSeat

allIds :: [Int]
allIds = [0..127*8]

missing :: [Int] -> [Int]
missing = (allIds \\)

identifySeat :: [Int] -> Int
identifySeat xs =
  head $ [m | (b:m:a:_) <- tails xs
            , b + 1 /= m
            , m + 1 /= a]

part2 = identifySeat . missing . map (seatId . findSeat)

example = "FBFBBFFRLR"

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
