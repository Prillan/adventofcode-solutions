{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

readDigit :: Num n => Char -> n
readDigit = \case '2' -> 2
                  '1' -> 1
                  '0' -> 0
                  '-' -> -1
                  '=' -> -2

readSnafu :: Num n => String -> n
readSnafu =
  sum
  . map (\(i, v) -> v * 5 ^ i)
  . zip @Int [0..]
  . map readDigit
  . reverse

decToSnafu :: Integral n => n -> String
decToSnafu = reverse . go 0
  where go carry n =
          case (carry + n) `divMod` 5 of
            (0, 0) -> []
            (rest, 0) -> '0':go 0 rest
            (rest, 1) -> '1':go 0 rest
            (rest, 2) -> '2':go 0 rest
            (rest, 3) -> '=':go 1 rest
            (rest, 4) -> '-':go 1 rest

parseAll :: Num n => String -> [n]
parseAll = map readSnafu  . lines

part1 :: [Int] -> String
part1 = decToSnafu . sum

main :: IO ()
main = main' "input.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   putStrLn (part1 input)
