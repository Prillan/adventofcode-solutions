import Data.Maybe (maybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

type Parser = Parsec Void String
num :: Parser Int
num = read <$> some digitChar

connectionP :: Parser (Int, [Int])
connectionP = do
  x1 <- num
  string " <-> "
  rest <- sepBy1 num (string ", ")
  pure $ (x1, rest)

parseAll = HashMap.fromList . map unsafeRight .
  map (parse connectionP "") . filter (not . null) . lines

collect :: Int -> HashMap Int [Int] -> Set Int
collect g input = collect' (Set.singleton g)
  where collect' s =
          let s' = Set.union s $ Set.fromList $ do
                e <- Set.toList s
                maybe [] id (HashMap.lookup e input)
          in
            if s' == s
            then s
            else collect' s'

part1 = length . collect 0
part2 input = length . Set.fromList . map (flip collect input) $ HashMap.keys input

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   -- Brute force, runs in ~2.5s when compiled with -O2
   print (part2 input)
