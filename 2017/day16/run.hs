{-# LANGUAGE BangPatterns #-}
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, update, (!?), (><))
import qualified Data.Sequence as Seq
import Text.Megaparsec

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

type Parser = Parsec Dec String
type Program = Char

data Action = Spin Int
            | Exchange Int Int
            | Partner Program Program
  deriving (Show, Eq)

numP = read <$> some digitChar

actionP = spinP <|> exchangeP <|> partnerP
spinP = Spin <$> (char 's' *> numP)
exchangeP = Exchange <$> (char 'x' *> numP) <*> (char '/' *> numP)
partnerP = Partner <$> (char 'p' *> anyChar) <*> (char '/' *> anyChar)

parser :: Parser [Action]
parser = sepBy1 actionP (char ',')

parseAll :: String -> [Action]
parseAll = unsafeRight
  . parse parser ""
  . head
  . lines

eval :: Seq Char -> Action -> Seq Char
eval s (Spin i) =
  let (first, second) = Seq.splitAt (length s - i) s
  in
    second >< first
eval s (Exchange i j) =
  case (s !? i, s !? j) of
    (Just !iv, Just !jv) -> update j iv . update i jv $ s
    _ -> error "Index out of range"
eval s (Partner p q) = fmap swap s
  where swap x
         | x == p    = q
         | x == q    = p
         | otherwise = x

dance :: Seq Char -> [Action] -> Seq Char
dance = foldl eval

part1 :: [Action] -> String
part1 = toList . dance (Seq.fromList ['a'..'p'])

orders :: [Action] -> [Seq Char]
orders input = iterate (flip dance input) (Seq.fromList ['a'..'p'])

findCycle :: [Seq Char] -> (Int, Int, Seq Char)
findCycle = findCycle' 0 Map.empty
  where findCycle' i m (x:xs) =
          case Map.lookup x m of
            Just j -> (j, i, x)
            Nothing -> findCycle' (i+1) (Map.insert x i m) xs

fpow :: (a -> a) -> Int -> (a -> a)
fpow f 1 = f
fpow f n = fpow f (n - 1) . f

part2 :: [Action] -> String
part2 input =
  let (start, end, val) = findCycle (orders input)
      rem = (10^9 - start) `mod` (end - start)
  in
    toList $ fpow (flip dance input) rem val

main = do
   input <- parseAll <$> readFile "input.txt"
   putStrLn (part1 input)
   putStrLn (part2 input)
