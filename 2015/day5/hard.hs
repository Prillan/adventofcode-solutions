import Data.List (isInfixOf)

nice s = cond1 && cond2
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

process = length . filter nice . lines

main = do
   input <- readFile "input.txt"
   print (process input)
