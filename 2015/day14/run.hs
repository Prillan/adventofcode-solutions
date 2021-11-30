import Data.List (maximumBy, groupBy, sortBy)
import Text.Parsec
import qualified Data.MultiSet as MS

data Raindeer = RD String Integer Integer Integer
  deriving (Show, Eq, Ord)

names = ["Dancer", "Cupid", "Rudolph", "Donner", "Dasher", "Blitzen", "Prancer", "Comet", "Vixen"]

number = (read :: String -> Integer) <$> many1 digit

raindeer = RD <$> (choice $ map (try.string) names)
              <*> (string " can fly " *> number)
              <*> (string " km/s for " *> number)
              <*> (string " seconds, but then must rest for " *> number)

unsafeRight (Right x) = x

distance :: Raindeer -> Integer -> Integer
distance (RD _ speed move rest) time = speed * (last + move*full)
  where (full, rest') = divMod time (move + rest)
        last = min move rest'

distances max deer = map (\i -> map (\d -> (d, distance d i)) deer) [1..max]

maximumsBy f = last . groupBy (\a b -> f a b == EQ) . sortBy f

part1 = maximum
        . map (flip distance 2503)
        . map unsafeRight
        . map (parse raindeer "") . lines

part2 = snd
        . maximumBy (\a b -> snd a `compare` snd b)
        . MS.toOccurList
        . MS.fromList
        . map (\(d, dist) -> (d, 1))
        . concat
        . map (maximumsBy (\a b -> snd a `compare` snd b))
        . distances 2503
        . map unsafeRight
        . map (parse raindeer "") . lines


main = do
   input <- readFile "input.txt"
   print (part1 input)
   print (part2 input)
