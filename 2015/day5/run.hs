import Data.List (isInfixOf)

vowles = "aeiou"
badSubstrings = ["ab", "cd", "pq", "xy"]

nice1 s =  length (filter (`elem` vowles) s) >= 3
       && null (filter (`elem` badSubstrings) $ zipWith (\x y -> [x, y]) s (tail s))
       && (not . null . filter id $ zipWith (==) s (tail s))

part1 = length . filter nice1 . lines

nice2 s = cond1 && cond2
  where cond1 = cond1' s
        cond1' [] = False
        cond1' [_] = False
        cond1' (x:y:xs)
          | isInfixOf [x, y] xs = True
          | otherwise = cond1' (y:xs)

        cond2 = cond2' s
        cond2' [] = False
        cond2' [x] = False
        cond2' [x, y] = False
        cond2' (x:y:z:xs) | x == z    = True
                          | otherwise = cond2' (y:z:xs)

part2 = length . filter nice2 . lines

main = do
   input <- readFile "input.txt"
   print (part1 input)
   print (part2 input)
