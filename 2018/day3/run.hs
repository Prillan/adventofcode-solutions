{-# LANGUAGE RecordWildCards #-}
import Data.Maybe
import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec (ErrorItem Char) String

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

type Grid = HashMap (Int, Int)

replace f d m k  =
  let v = maybe d id (HashMap.lookup k m)
  in HashMap.insert k (f v) m

data Claim = Claim { cId :: Int
                   , cX  :: Int
                   , cY  :: Int
                   , cW  :: Int
                   , cH  :: Int }
  deriving (Show, Eq)

int :: Parser Int
int = read <$> many digitChar

claimp = Claim
  <$> (char '#' *> int)
  <*> (string " @ " *> int)
  <*> (char ',' *> int)
  <*> (string ": " *> int)
  <*> (char 'x' *> int)

parseAll =
  map unsafeRight .
  map (parse claimp "") . lines

cross :: [a] -> [b] -> [(a, b)]
cross xs ys = (,) <$> xs <*> ys

claimArea :: Claim -> Grid Int -> Grid Int
claimArea Claim{..} m =
  foldl (replace (+1) 0) m (cross [cX..cX+cW-1] [cY..cY+cH-1])

part1 :: [Claim] -> Int
part1 = length
        . filter ((>1) . snd)
        . HashMap.toList
        . foldl (flip claimArea) HashMap.empty

overlaps :: Claim -> Claim -> Bool
overlaps c1 c2 =
  let l1 = cX c1
      r1 = cX c1 + cW c1
      t1 = cY c1
      b1 = cY c1 + cH c1
      l2 = cX c2
      r2 = cX c2 + cW c2
      t2 = cY c2
      b2 = cY c2 + cH c2
  in
    l1 < r2 && r1 > l2 && t1 < b2 && b1 > t2

part2 :: [Claim] -> Int
part2 input = cId . head . filter f $ input
  where f c = all (not . overlaps c) $ filter ((/= cId c) . cId) input

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
