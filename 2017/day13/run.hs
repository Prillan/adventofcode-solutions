import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

type Parser = Parsec Void String

num :: Parser Int
num = read <$> some digitChar

layerP :: Parser (Int, Int)
layerP = (,) <$> num <*> (string ": " *> num)

parseAll :: String -> [(Int, Int)]
parseAll = map unsafeRight
  . map (parse layerP "")
  . filter (not . null)
  . lines

caughtInLayer :: Int -> Int -> Int -> Bool
caughtInLayer s l d = (s + l) `mod` (2 * (d - 1)) == 0

tripWeight :: Int -> [(Int, Int)] -> Int
tripWeight startAt = sum . map weight
  where weight (i, d)
          | caughtInLayer startAt i d = i * d
          | otherwise = 0

caught :: Int -> [(Int, Int)] -> Bool
caught startAt = any (uncurry (caughtInLayer startAt))

part1 :: [(Int, Int)] -> Int
part1 = tripWeight 0

part2 :: [(Int, Int)] -> Int
part2 input = head $ filter (not . flip caught input) $ [0..]

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
