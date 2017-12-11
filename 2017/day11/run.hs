import Control.Applicative (liftA2)

data Coord a = C !a !a !a
  deriving (Show, Eq)

instance Functor Coord where
  fmap f (C c1 c2 c3) = C (f c1) (f c2) (f c3)

instance Applicative Coord where
  pure v = C v v v
  (C f1 f2 f3) <*> (C v1 v2 v3) = C (f1 v1) (f2 v2) (f3 v3)

instance Num a => Num (Coord a) where
  (+) = liftA2 (+)
  fromInteger = pure . fromInteger

toCoord :: String -> Coord Int
toCoord "s"  = C   0 (-1)   1
toCoord "n"  = C   0   1  (-1)
toCoord "se" = C   1 (-1)   0
toCoord "nw" = C (-1)  1    0
toCoord "sw" = C (-1)  0    1
toCoord "ne" = C   1   0  (-1)
toCoord x = error $ "Invalid direction: " ++ x

parseAll :: String -> [Coord Int]
parseAll = map toCoord . go . head . lines
  where go [] = []
        go [x] = [[x]]
        go [x, y] = [[x, y]]
        go (x:',':xs) = [x]:go xs
        go (x:y:',':xs) = [x, y]:go xs

dist :: Coord Int -> Int
dist (C x y _) = abs $ x + y

part1 :: [Coord Int] -> Int
part1 = dist . sum

part2 :: [Coord Int] -> Int
part2 = maximum . map dist . scanl (+) 0

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
