part1 = sum . map f . filter (`elem` ['(', ')'])
  where f x | x == '(' = 1
            | x == ')' = -1

part2 = fst
        . head
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
  print (part1 input)
  print (part2 input)
