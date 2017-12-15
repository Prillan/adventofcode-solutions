import Data.Bits ((.&.))

factorA, factorB :: Integer
factorA = 16807
factorB = 48271

multiplesA, multiplesB :: Integer
multiplesA = 4
multiplesB = 8

generate :: Integer -> Integer -> [Integer]
generate seed factor =
  iterate (\x -> (x * factor) `mod` (2 ^ 31 - 1)) seed

equal :: [Integer] -> [Integer] -> [Bool]
equal genA genB = drop 1 $ zipWith (==) (map first16 genA) (map first16 genB)
  where first16 = ((2^16 - 1) .&.)

part1 :: Integer -> Integer -> Int
part1 seedA seedB = length
  . filter id
  . take (40 * 10^6)
  $ equal (generate seedA factorA) (generate seedB factorB)

part2 :: Integer -> Integer -> Int
part2 seedA seedB = length
  . filter id
  . take (5 * 10^6)
  $ equal (filter (multipleOf multiplesA) (generate seedA factorA))
          (filter (multipleOf multiplesB) (generate seedB factorB))
  where multipleOf n x = x `mod` n == 0


main = do
  let inputA = 591
      inputB = 393
  print (part1 inputA inputB)
  print (part2 inputA inputB)
