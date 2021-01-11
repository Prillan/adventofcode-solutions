{-# LANGUAGE RecordWildCards #-}
import Data.Maybe
import Text.Parsec

data Box = Box { w :: Integer, h :: Integer, l :: Integer}
  deriving Show

measure = read <$> many1 digit

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

readBox :: String -> Maybe Box
readBox = eitherToMaybe . parse box ""
  where box = Box <$> measure
                  <*> (char 'x' *> measure)
                  <*> (char 'x' *> measure)

paper :: Box -> Integer
paper Box{..} = sum (map (2*) sides) + minimum sides
  where sides = [w*l, l*h, w*h]

ribbon :: Box -> Integer
ribbon Box{..} = minimum sides + (w*l*h)
    where sides = map (*2) [w+l, l+h, w+h]

part1 = sum . map paper . mapMaybe readBox . lines
part2 = sum . map ribbon . mapMaybe readBox . lines

main = do
  input <- readFile "input.txt"
  print (part1 input)
  print (part2 input)
