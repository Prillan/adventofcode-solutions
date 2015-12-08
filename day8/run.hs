
readString s = readString' . init . tail $ s
  where readHex x = read ("'\\x" ++ x ++ "'") :: Char
        readString' [] = []
        readString' [x] = [x]
        readString' (x:y:xs)
           | x == '\\' = case y of
                           'x' -> (readHex (take 2 xs)):readString' (drop 2 xs)
                           '\\' -> '\\':readString' xs
                           '"' -> '"':readString' xs
           | otherwise = x:readString' (y:xs)

process input = (sum . map length $ ls) - (sum . map length . map readString $ ls)
  where ls = lines input

main = readFile "input.txt" >>= print . process
