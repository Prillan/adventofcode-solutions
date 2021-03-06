{-# LANGUAGE TupleSections #-}
import           Data.List (permutations, group, minimum, maximum, minimumBy, maximumBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.Megaparsec

width :: Int
width = 50
height :: Int
height = 6

type Screen = Map (Int, Int) Bool

printScreen :: Screen -> String
printScreen scr =
  unlines
  . map (map (\(x, y) -> printChar (Map.lookup (x, y) scr)))
  . map (\y -> (,y) <$> [0..width-1]) $  [0..height-1]
  where printChar (Just True) = 'X'
        printChar _           = ' '

data Inst = Rect Int Int
          | RotRow Int Int
          | RotCol Int Int
 deriving Show

parseInst :: Parsec Dec String Inst
parseInst = rect <|> row <|> col
  where num = (read :: String -> Int) <$> some digitChar
        rect = Rect <$> (string "rect " *> num)
                    <*> (char 'x' *> num)
        row = RotRow <$> (string "rotate row y=" *> num)
                     <*> (string " by " *> num)
        col = RotCol <$> (string "rotate column x=" *> num)
                     <*> (string " by " *> num)

unsafeRight (Right x) = x

parseAll = map unsafeRight .
  map (parse parseInst "") . lines

update :: Inst -> Screen -> Screen
update (Rect w h) prev = Map.union rect prev
  where rect = Map.fromList $ [((x, y), True) | x <- [0..w - 1], y <- [0..h - 1]]
update (RotRow y step) prev = foldr f prev (zip (map ((`mod` width) . (+step)) r) r)
  where r = [0..width - 1]
        f (xn, xo) m =
          case Map.lookup (xo, y) prev of
            Just v  -> Map.insert (xn, y) v m
            Nothing -> Map.insert (xn, y) False m
update (RotCol x step) prev = foldr f prev (zip (map ((`mod` height) . (+step)) r) r)
  where r = [0..height - 1]
        f (yn, yo) m =
          case Map.lookup (x, yo) prev of
            Just v  -> Map.insert (x, yn) v m
            Nothing -> Map.insert (x, yn) False m

exec :: [Inst] -> Screen
exec = foldl (flip update) Map.empty

part1 :: [Inst] -> Integer
part1 = Map.foldl (\a v -> if v then a+1 else a) 0 . exec

part2 :: [Inst] -> String
part2 = printScreen . exec

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   putStrLn (part2 input)
