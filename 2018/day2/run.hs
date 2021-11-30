import Data.Bits (xor)
import Data.Char (ord)
import Data.Maybe
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

-- unsafeRight :: Show a => Either a b -> b
-- unsafeRight (Right x) = x
-- unsafeRight (Left x) = error $ show x

replace m k f d =
  let v = maybe d id (Map.lookup k m)
  in Map.insert k (f v) m

parseAll = lines

part1 = uncurry (*) . foldl f (0, 0)
  where f (d, t) i =
          let counts = map snd . Map.toList $ countChars i
          in
            ( d + (if 2 `elem` counts then 1 else 0)
            , t + (if 3 `elem` counts then 1 else 0))

countChars :: String -> Map Char Int
countChars = foldl go Map.empty
  where go :: Map Char Int -> Char -> Map Char Int
        go m c = replace m c (+1) 0

part2 input =
  let match = head
            . filter ((== 1) . uncurry diff)
            $ [(x, y) | x <- input, y <- input]

  in
    map fst $ filter (uncurry (==)) $ uncurry zip match
  where diff :: String -> String -> Int
        diff x y = sum $ zipWith f x y
        f c d
          | c == d    = 0
          | otherwise = 1

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   putStrLn (part2 input)
