{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)

largest3 :: [Int] -> [Int]
largest3 = \case a:b:c:rest ->
                   let [a',b',c'] = sort [a,b,c]
                   in go a' b' c' rest
                 xs -> sort xs
  where -- a <= b <= c
        go a b c [] = [a,b,c]
        go !a !b !c (!x:xs)
          | x >= c    = go b c x xs
          | x >= b    = go b x c xs
          | x >= a    = go x b c xs
          | otherwise = go a b c xs

parse :: BL.ByteString -> Int
parse =
  (\case (Right (x, _)) -> x)
  . decimal
  . decodeUtf8
  . BL.toStrict

parseAll :: BL.ByteString -> [[Int]]
parseAll =
  map (map parse)
  . filter (not . null)
  . splitOn [BL.empty]
  . BL.split 0x0a

main :: IO ()
main =
  main' "input.txt"

process :: BL.ByteString -> [Int]
process = largest3 . map sum . parseAll

main' :: FilePath -> IO ()
main' file = do
   [x1, x2, x3] <- process <$> BL.readFile file
   print $ x3
   print $ x1 + x2 + x3
