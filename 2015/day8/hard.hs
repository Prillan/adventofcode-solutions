
process input = (length . concat . map show $ ls) - (sum . map length $ ls)
  where ls = lines input

main = do
   input <- readFile "input.txt"
   print (process input)
