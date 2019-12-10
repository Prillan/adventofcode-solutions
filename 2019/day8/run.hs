import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Ord (comparing)
import Data.List (minimumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Counter a = Map a Int

data Color = Black | White
  deriving (Show, Eq)

color :: Int -> Maybe Color
color 0 = Just Black
color 1 = Just White
color _ = Nothing

pretty :: Maybe Color -> Char
pretty (Just Black) = '█'
pretty (Just White) = ' '
pretty _ = 'X'

counter :: Ord a => [a] -> Counter a
counter = Map.fromListWith (+) . flip zip (repeat 1)

count :: Ord a => a -> Counter a -> Int
count x m =
  case Map.lookup x m of
    Just v  -> v
    Nothing -> 0

batch :: Int -> [a] -> [[a]]
batch _ [] = []
batch i l = let (x, y) = splitAt i l in x:batch i y

parseAll :: String -> [Int]
parseAll = map (read . pure) . filter isDigit

decompose :: Int -> Int -> [a] -> [[[a]]]
decompose w h = map (batch w) . batch (w * h)

part1 :: Int -> Int -> [Int] -> Int
part1 w h =
  ((*) <$> count 1 <*> count 2)
  . minimumBy (comparing (count 0))
  . map (counter . concat)
  . decompose w h

part2 :: Int -> Int -> [Int] -> String
part2 w h =
  unlines
  . batch w
  . map pretty
  . foldl1 (zipWith (<|>))
  . map concat
  . decompose w h
  . map color


main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 25 6 input)
   putStrLn (part2 25 6 input)
