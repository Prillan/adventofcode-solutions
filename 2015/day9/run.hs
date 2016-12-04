{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.List (minimumBy, permutations, nub)
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString, fromString)
import Text.Parsec hiding ((<|>))

import Control.Applicative ((<|>))

newtype City = City String
  deriving (Show, Read, Eq, IsString, Ord)

type Distance = Integer
data Path = Path City City Distance
  deriving Show

start (Path c _ _) = c
stop (Path _ c _) = c
dist (Path _ _ d) = d

city = fromString <$> many1 (oneOf az)
  where az = ['A'..'Z'] ++ ['a'..'z']

number = fromInteger . read <$> many1 digit

path = Path <$> city <*> (string " to " *> city) <*> (string " = " *> number)

readPath s = case parse path "" s of
               Right x -> Just x
               Left _ -> Nothing

process = shortestPath . mapMaybe readPath . lines

type Result = ([City], Distance)

shortestPath :: [Path] -> Result
shortestPath paths = minimumBy (\a b -> snd a `compare` snd b)
                   . map (\cs -> (cs, sum $ zipWith lookup cs (drop 1 cs)))
                   . permutations $ cities
  where cities = nub $ map start paths ++ map stop paths
        distances = Map.fromList . map (\p -> ((start p, stop p), dist p)) $ paths
        lookup s e = fromJust $ Map.lookup (s, e) distances <|> Map.lookup (e, s) distances

remove :: Eq a => a -> [a] -> [a]
remove x = filter (/= x)

main = do
   input <- readFile "input.txt"
   print (process input)
