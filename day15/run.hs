config = [
  (1, 7, 0),
  (2, 13, 0),
  (3, 3, 2),
  (4, 5, 2),
  (5, 17, 0),
  (6, 19, 7)
  ]

example = [
  (1, 5, 4),
  (2, 2, 1)
 ]

exprs :: [(Integer, Integer, Integer)] -> [Integer -> Bool]
exprs = map (\(d, m, s) ->
               \t -> (d + s + t) `mod` m == 0)
part1 =
  let e = exprs config
  in
    filter (\x -> all ($ x) e) [0..]
part2 =
  let e = exprs $ config ++ [(7, 11, 0)]
  in
    filter (\x -> all ($ x) e) [0..]

main = do
   print (head part1)
   print (head part2)
