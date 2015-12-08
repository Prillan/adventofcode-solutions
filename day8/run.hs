
readString s = readString' . init . tail $ s
  where readHex x = read ("'\\x" ++ x ++ "'") :: Char
        readString' [] = []
        readString' [x] = [x]
        readString' ('\\':'x':xs) =
          readHex (take 2 xs):readString' (drop 2 xs)
        readString' ('\\':y:xs) = y:readString' xs
        readString' (x:xs) = x:readString' xs

process input = (sum . map length $ ls) - (sum . map length . map readString $ ls)
  where ls = lines input

main = readFile "input.txt" >>= print . process
