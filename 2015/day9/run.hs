{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative ((<|>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List (minimumBy, permutations, nub)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString, fromString)
import Text.Parsec hiding ((<|>))

newtype City = City String
  deriving (Show, Read, Eq, IsString, Ord, Hashable)

type Distance = Integer
data Path = Path { start :: City
                 , stop  :: City
                 , dist  :: Distance
                 }
  deriving Show

city = fromString <$> many1 (oneOf az)
  where az = ['A'..'Z'] ++ ['a'..'z']

number = fromInteger . read <$> many1 digit

path = Path <$> city <*> (string " to " *> city) <*> (string " = " *> number)

readPath s = case parse path "" s of
               Right x -> Just x
               Left _ -> Nothing

parseAll = mapMaybe readPath . lines

type Result = ([City], Distance)

shortestPath :: [Path] -> Result
shortestPath paths = minimumBy (\a b -> snd a `compare` snd b)
                   . map (\cs -> (cs, sum $ zipWith lookup cs (drop 1 cs)))
                   . permutations $ cities
  where cities = nub $ map start paths ++ map stop paths
        distances = HashMap.fromList . map (\p -> ((start p, stop p), dist p)) $ paths
        lookup s e = fromJust $ HashMap.lookup (s, e) distances <|> HashMap.lookup (e, s) distances

remove :: Eq a => a -> [a] -> [a]
remove x = filter (/= x)

part1 = snd . shortestPath
part2 = negate . snd . shortestPath . map (\p -> p { dist = negate (dist p) })

main = do
   input <- parseAll <$> readFile "input.txt"
   print $ part1 input
   print $ part2 input
