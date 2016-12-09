import Text.Megaparsec

num :: Parsec Dec String Int
num = read <$> some digitChar

marker :: Parsec Dec String (Int, Int)
marker = between (char '(')
                 (char ')')
                 ((,) <$> (num <* char 'x') <*> num)

markerLength :: (Int, Int) -> Int
markerLength (x, y) = length (show x ++ show y) + 3

decompressedLength :: String -> Int -> Int
decompressedLength [] acc = acc
decompressedLength s@(_:rest) acc =
  case parse marker "" s of
    Right (len, times) ->
      let mlen = markerLength (len, times)
          next = drop (len + mlen) s
      in
        decompressedLength next (acc + len * times)
    Left _ ->
      decompressedLength rest (acc + 1)

part1 :: String -> Int
part1 = flip decompressedLength 0

decl2 :: String -> Integer
decl2 [] = 0
decl2 s =
  case parse marker "" s of
    Right (len, times) ->
      let mlen = markerLength (len, times)
          noMarker = drop mlen s
          marked = take len noMarker
      in
        (fromIntegral times * decl2 marked) + decl2 (drop len noMarker)
    Left _ ->
      1 + decl2 (drop 1 s)

part2 :: String -> Integer
part2 = decl2

main :: IO ()
main = do
   input <- filter (/= '\n') <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
