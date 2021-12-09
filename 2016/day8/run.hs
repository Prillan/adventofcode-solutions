{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
import AoC
import AoC.Draw.Chars
import AoC.Grid

import           Data.List (permutations, group, minimum, maximum, minimumBy, maximumBy)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

width :: Int
width = 50
height :: Int
height = 6

type Screen = MapGrid Bool

printScreen :: Screen -> String
printScreen = ppMapGrid (\case True -> 'X'
                               _    -> ' ')

data Inst = Rect Int Int
          | RotRow Int Int
          | RotCol Int Int
 deriving Show

parseInst :: Parsec Void String Inst
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
update (Rect w h) prev = HashMap.union rect prev
  where rect = HashMap.fromList $ [((x, y), True) | x <- [0..w - 1], y <- [0..h - 1]]
update (RotRow y step) prev = foldr f prev (zip (map ((`mod` width) . (+step)) r) r)
  where r = [0..width - 1]
        f (xn, xo) m =
          case HashMap.lookup (xo, y) prev of
            Just v  -> HashMap.insert (xn, y) v m
            Nothing -> HashMap.insert (xn, y) False m
update (RotCol x step) prev = foldr f prev (zip (map ((`mod` height) . (+step)) r) r)
  where r = [0..height - 1]
        f (yn, yo) m =
          case HashMap.lookup (x, yo) prev of
            Just v  -> HashMap.insert (x, yn) v m
            Nothing -> HashMap.insert (x, yn) False m

exec :: [Inst] -> Screen
exec = foldl (flip update) HashMap.empty

part1 :: [Inst] -> Int
part1 = HashMap.foldl (\a v -> a + fromEnum v) 0 . exec

part2 :: [Inst] -> String
part2 = unsafeRight . readLetters . printScreen . exec

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   putStrLn (part2 input)
