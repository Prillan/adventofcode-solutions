import Data.Foldable
import Data.Coerce
import Data.Bool
import Data.Bits (xor)
import Data.Maybe
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Semigroup
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

rowP :: Parser [Bool]
rowP = some $ (char '.' *> pure False) <|> (char '#' *> pure True)

gridP :: Parser Grid
gridP = Grid . fromLists <$> sepBy1 rowP (char '/')

ruleP :: Parser (Grid, Grid)
ruleP = do
  input <- gridP
  string " => "
  output <- gridP
  pure (input, output)

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

newtype Grid = Grid (Seq (Seq Bool))
  deriving (Eq, Ord)

toLists = fmap toList . toList
fromLists = Seq.fromList . map Seq.fromList

instance Show Grid where
  show (Grid g) =
    show . intercalate "/" $ map (map (bool '.' '#') . toList) (toList g)

display (Grid x) = unlines $ map (map (bool '.' '#') . toList) (toList x)

slice :: Int -> Int -> Int -> Int -> Grid -> Grid
slice r0 r1 c0 c1 (Grid g) =
  Grid $ Seq.take r1 . Seq.drop r0 . fmap (Seq.take c1 . Seq.drop c0) $ g

unslice :: Grid -> Grid -> Grid -> Grid -> Grid
unslice tl tr bl br = glueV (glueH tl tr) (glueH bl br)

glueV :: Grid -> Grid -> Grid
glueV (Grid g0) (Grid g1) = Grid $ g0 <> g1

glueManyV :: Foldable t => t Grid -> Grid
glueManyV = foldl glueV (Grid Seq.empty)

glueH :: Grid -> Grid -> Grid
glueH (Grid g0) (Grid g1)
  | Seq.null g0 = Grid g1
  | Seq.null g1 = Grid g0
  | otherwise = Grid $ Seq.zipWith (<>) g0 g1

glueManyH :: Foldable t => t Grid -> Grid
glueManyH = foldl glueH (Grid Seq.empty)

transpose' (Grid g) =
  Grid $ fromLists $ transpose . toLists $ g

flipX (Grid g) = Grid $ Seq.reverse g
flipY (Grid g) = Grid $ fmap Seq.reverse g

rotateCW  = flipY . transpose'
rotateCCW = rotateCW . rotateCW . rotateCW

symmetries =
  [ id
  , rotateCW
  , rotateCW . rotateCW
  , rotateCCW
  , flipX
  , flipX . rotateCW
  , flipX . rotateCW . rotateCW
  , flipX . rotateCCW ]

split :: Grid -> Seq (Seq Grid)
split (Grid g) =
  let size = length g
      d = if size `mod` 2 == 0
            then 2
            else 3
  in
    fromLists [ [ slice r d c d (Grid g) | c <- [0,d..size-1] ]
              | r <- [0,d..size-1] ]

expand rules g =
  case Map.lookup (asKey g) rules of
    Just o -> o
    _ -> g

asKey (Grid g) = (length g, concat $ toLists g)

variations rules = concatMap variations' rules
  where variations' (input, output) =
          [ (asKey (sym input), output) | sym <- symmetries ]

enhance rules = fmap (fmap (expand rules))

merge = glueManyV . fmap glueManyH

step rules = merge . enhance rules . split

parseAll =
  map unsafeRight .
  map (parse ruleP "") . lines

start = unsafeRight $ parse gridP "" ".#./..#/###"

on :: Grid -> Int
on (Grid g) = length . filter id . concat . map toList . toList $ g

buildRules = Map.fromList . variations

solve i input = on $ iterate (step (buildRules input)) start !! i

part1 = solve 5
part2 = solve 18

main = do
   input <- parseAll <$> readFile "input.txt"
   print $ part1 input
   print $ part2 input
