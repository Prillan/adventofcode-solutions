{-# LANGUAGE TypeApplications #-}
import Data.Char (isDigit)
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

-- byr (Birth Year)
-- iyr (Issue Year)
-- eyr (Expiration Year)
-- hgt (Height)
-- hcl (Hair Color)
-- ecl (Eye Color)
-- pid (Passport ID)
-- cid (Country ID)

fields =
  [ "byr"
  , "iyr"
  , "eyr"
  , "hgt"
  , "hcl"
  , "ecl"
  , "pid"
  , "cid" ]

required = init fields

parseLineFields :: String -> [(String, String)]
parseLineFields =
  map parseLineField
  . words

parseLineField f =
  case span (/= ':') f of
    (fname, ':':value) -> (fname, value)

parse :: [String] -> [[(String, String)]]
parse = go [] Nothing
  where go acc (Just c) [] = reverse $ c:acc
        go acc curr (f:rest) =
          case (curr, parseLineFields f) of
            (Just c,  []) -> go (c:acc) Nothing rest
            (Nothing, []) -> go acc Nothing rest
            (Just c,  fs) -> go acc (Just (c ++ fs)) rest
            (Nothing, fs) -> go acc (Just fs) rest

parseAll =
  parse . lines

hasAllRequired pp = all (`elem` (map fst pp)) required

between v l h = l <= v && v <= h

validate "byr" v = between (read @Int v) 1920 2002
validate "iyr" v = between (read @Int v) 2010 2020
validate "eyr" v = between (read @Int v) 2020 2030
validate "hgt" v =
  case reads @Int v of
    [(v', "cm")] -> between v' 150 193
    [(v', "in")] -> between v' 59 76
    _ -> False
validate "hcl" ('#':v) = all (`elem` "0123456789abcdef") v
validate "ecl" v = v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validate "pid" v = length v == 9 && all isDigit v
validate "cid" _ = True
validate _ _ = False

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:

--     If cm, the number must be at least 150 and at most 193.
--     If in, the number must be at least 59 and at most 76.

-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.

valid pp = hasAllRequired pp && all (uncurry validate) pp

part1 = length . filter hasAllRequired
part2 = length . filter valid

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
