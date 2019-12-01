parseAll =
  map read . lines

part1 :: [Integer] -> Integer
part1 = sum . map f
  where f x = (x `div` 3) - 2

part2 :: [Integer] -> Integer
part2 = sum . extend
  where f x = (x `div` 3) - 2
        extend [] = []
        extend input =
          let processed = filter (> 0) (map f input)
          in
            processed ++ extend processed

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
