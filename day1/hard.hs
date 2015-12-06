
process = take 1
        . filter ((== -1) . snd)
        . zip [0..]
        . scanl (+) 0
        . map f
        . filter (`elem` ['(', ')'])
  where f :: Char -> Integer
        f x | x == '(' = 1
            | x == ')' = -1

main = do
   input <- readFile "input.txt"
   print (process input)
