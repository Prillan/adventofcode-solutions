

vowles = "aeiou"
badSubstrings = ["ab", "cd", "pq", "xy"]

nice s =  length (filter (`elem` vowles) s) >= 3
       && null (filter (`elem` badSubstrings) $ zipWith (\x y -> [x, y]) s (tail s))
       && (not . null . filter id $ zipWith (==) s (tail s))

process = length . filter nice . lines

main = do
   input <- readFile "input.txt"
   print (process input)
