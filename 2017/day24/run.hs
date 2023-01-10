{-# LANGUAGE BangPatterns #-}
import AoC.Parse (numP)
import Data.Ord (comparing)
import Data.List (maximumBy, nub, sort)
import Data.Semigroup ((<>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

type N = Int
data Component = C !N -- High
                   !N -- Low

instance Show Component where
  show (C x y) = show x ++ "/" ++ show y

instance Eq Component where
  (C x1 x2) == (C y1 y2) = x1 == y1 && x2 == y2

instance Ord Component where
  compare (C x1 x2) (C y1 y2) = compare x1 y1 <> compare x2 y2

componentP :: Parser Component
componentP = do
  x <- numP
  char '/'
  y <- numP
  pure $ C (max x y) (min x y)

has :: N -> Component -> Bool
has z (C x y) = x == z || y == z

val :: Component -> N
val (C x y) = x + y

other :: N -> Component -> N
other v (C x y)
  | v == x    = y
  | otherwise = x

parseAll :: String -> [Component]
parseAll =
  map unsafeRight .
  map (parse componentP "") . lines

check :: [Component] -> Bool
check comps = length comps == length (nub comps)

solve :: [Component] -> [(Int, N)]
solve initial = go (reverse (sort initial)) 0
  where go :: [Component] -> N -> [(Int, N)]
        go !remaining !prev =
          case filter (has prev) remaining of
            [] -> [(0, 0)]
            candidates -> do
              c <- candidates
              let remaining' = filter (/= c) remaining
              (!l, !s) <- go remaining' (other prev c)
              pure $ (l + 1, val c + s)

part1 :: [(Int, N)] -> N
part1 = snd . maximumBy (comparing snd)

part2 :: [(Int, N)] -> N
part2 = snd . maximum

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   if check input
     then do
       let bridges = solve input
       print (part1 bridges)
       print (part2 bridges)
     else error "Check failed!"
