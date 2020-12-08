{-# LANGUAGE TypeApplications #-}


parseAll =
  map (read @Int) . lines

part1 xs = head [x * y | x <- xs
                       , y <- xs
                       , x /= y
                       , x + y == 2020]

part2 xs = head [x * y * z | x <- xs
                           , y <- xs
                           , z <- xs
                           , x /= y
                           , x + y < 2020
                           , y /= z
                           , x /= z
                           , x + y + z == 2020]

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
