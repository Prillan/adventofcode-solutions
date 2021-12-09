import Data.Bits (xor)
import Data.Char (ord)
import Data.Maybe
import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

-- unsafeRight :: Show a => Either a b -> b
-- unsafeRight (Right x) = x
-- unsafeRight (Left x) = error $ show x

replace m k f d =
  let v = maybe d id (HashMap.lookup k m)
  in HashMap.insert k (f v) m

parseAll = lines

part1 = uncurry (*) . foldl f (0, 0)
  where f (d, t) i =
          let counts = map snd . HashMap.toList $ countChars i
          in
            ( d + (if 2 `elem` counts then 1 else 0)
            , t + (if 3 `elem` counts then 1 else 0))

countChars :: String -> HashMap Char Int
countChars = foldl go HashMap.empty
  where go :: HashMap Char Int -> Char -> HashMap Char Int
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
