
trig n = (n * (n+1)) `div` 2
toN (r, c) = trig d + c
  where d = (c - 1) + (r - 1)

code = code' . toN
code' n = (20151125 * modExp 252533 (n - 1) 33554393) `mod` 33554393

modExp b e m
  | e == 1 = b `mod` m
  | even e = modExp (b^2 `mod` m) (e `div` 2) m
  | odd e  = (b * modExp (b^2 `mod` m) (e `div` 2) m) `mod` m

input = (2981, 3075)

part1 = code

main = print (part1 input)
