{-# LANGUAGE TypeApplications #-}
import Data.Char (isDigit)
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

parseLine = pure

parse :: [String] -> [[String]]
parse = go [] Nothing
  where go acc (Just c) [] = reverse $ c:acc
        go acc curr (f:rest) =
          case (curr, parseLine f) of
            (Just c,  [""]) -> go (c:acc) Nothing rest
            (Nothing, [""]) -> go acc Nothing rest
            (Just c,  fs) -> go acc (Just (c ++ fs)) rest
            (Nothing, fs) -> go acc (Just fs) rest

parseAll :: String -> [[String]]
parseAll =
  parse . lines

anyone = nub . concat
everyone = foldl1 Set.intersection . map Set.fromList

part1 =
  sum . map (length . anyone)
part2 =
  sum . map (length . everyone)

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
