{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

import AoC
import Data.Ord (comparing)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map.Strict as Map

type Parser = Parsec Void String

data Raindeer = RD String Int Int Int
  deriving (Show, Eq, Ord)

names :: [String]
names = [ "Dancer"
        , "Cupid"
        , "Rudolph"
        , "Donner"
        , "Dasher"
        , "Blitzen"
        , "Prancer"
        , "Comet"
        , "Vixen"
        ]

number :: Parser Int
number = read <$> many digitChar

raindeer :: Parser Raindeer
raindeer = RD <$> (choice $ map (try.string) names)
              <*> (string " can fly " *> number)
              <*> (string " km/s for " *> number)
              <*> (string " seconds, but then must rest for " *> number)

distance :: Raindeer -> Int -> Int
distance (RD _ speed move rest) time = speed * (last + move*full)
  where (full, rest') = divMod time (move + rest)
        last = min move rest'

distances :: Int -> [Raindeer] -> [[(Raindeer, Int)]]
distances maxDistance deer =
  map (\i -> map (\d -> (d, distance d i)) deer) [1..maxDistance]

maximumsBy :: (a -> a -> Ordering) -> [a] -> [a]
maximumsBy _ [] = error "maximum of empty list"
maximumsBy f (x:xs) = go [x] xs
  where go acc@(a:_) =
          \case []     -> acc
                (b:bs) ->
                  case f a b of
                    LT -> go [b] bs
                    EQ -> go (b:acc) bs
                    GT -> go acc bs

parseAll :: String -> [Raindeer]
parseAll = unsafeRight . traverse (parse raindeer "") . lines

unsafeRight (Right x) = x

part1 :: [Raindeer] -> Int
part1 = maximum
        . map (flip distance 2503)

part2 :: [Raindeer] -> Int
part2 = maximum
        . map snd
        . Map.toList
        . counter
        . map fst
        . concat
        . map (maximumsBy (comparing snd))
        . distances 2503


main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
