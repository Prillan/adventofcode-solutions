

process = sum . map f . filter (`elem` ['(', ')'])
  where f x | x == '(' = 1
            | x == ')' = -1

main = do
   input <- readFile "input.txt"
   print (process input)
