{-# LANGUAGE RecordWildCards #-}
import           Control.Arrow ((&&&))
import           Data.Char (ord, chr)
import           Data.List (intercalate, sortOn, isInfixOf)
import           Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import           Text.Parsec

data Room = Room { name :: [String]
                 , sector :: Int
                 , checksum :: String }
  deriving (Show)

number = (read :: String -> Int) <$> many1 digit

parseRoom =
  Room <$> (many1 (many1 letter <* char '-'))
       <*> number
       <*> between (char '[') (char ']') (many1 letter)

unsafeRight (Right x) = x

parseAll = map unsafeRight .
  map (parse parseRoom "") . lines

validate :: Room -> Bool
validate Room {..} =
  (== checksum)
  . map fst
  . take 5
  . sortOn swap
  . MS.toOccurList
  . MS.fromList $ concat name

  where swap (x, y) = (-y, x)

decrypt :: Room -> String
decrypt Room {..} = intercalate " " . map (map decrypt') $ name
  where decrypt' = chr . (+ 97) . (`mod` 26) . (+ sector) . (+ (negate 97)) . ord

part1 = sum . map sector . filter validate
part2 = map (sector . snd) . filter (isInfixOf "north" . fst) . map (decrypt &&& id) . filter validate

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
