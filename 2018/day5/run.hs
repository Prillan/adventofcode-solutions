import Data.Char (isLower, isUpper, toLower, toUpper)

mergeable :: Char -> Char -> Bool
mergeable x y =
  (isLower x && isUpper y && toUpper x == y) ||
  (isUpper x && isLower y && toLower x == y)

fixpoint :: Eq t => (t -> t) -> t -> t
fixpoint f a = go a (f a)
  where go x fx
          | x == fx   = x
          | otherwise = go fx (f fx)

fullyReact :: String -> String
fullyReact = fixpoint react

react :: Foldable t => t Char -> [Char]
react s =
  let (acc, r) = foldl f ([], Nothing) s
  in
    reverse $ maybe [] pure r ++ acc

  where f (acc, Nothing) y = (acc, Just y)
        f (acc, Just x) y
          | mergeable x y = (acc, Nothing)
          | otherwise = (x:acc, Just y)

part1 :: [Char] -> Int
part1 = length . fullyReact

part2 :: [Char] -> Int
part2 input = minimum
  . map length
  . map fullyReact
  . map (\c -> filter ((c /=) . toLower) input)
  $ ['a'..'z']

main :: IO ()
main = do
   input <- head . lines <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
