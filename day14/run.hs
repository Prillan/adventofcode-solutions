import Data.List (maximumBy)
import Text.Parsec

data Raindeer = RD String Integer Integer Integer
  deriving Show

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

process = --maximumBy (\a b -> compare (distance a 2503) (distance b 2503))
        maximum . map (flip distance 2503)
        . map unsafeRight
        . map (parse raindeer "") . lines

main = do
   input <- readFile "input.txt"
   print (process input)
