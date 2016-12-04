import Data.Coerce
import Data.List ((\\))
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Password = Password (Vector Int)

strRep :: Password -> String
strRep = V.toList . V.map (az !!) . coerce
fromStrRep = Password . V.fromList . mapMaybe (`Map.lookup` rev)

instance Show Password where
  show p = strRep p ++ " " ++ s ++ " " ++ show (toInt p)
    where s = (show :: [Int] -> String) . V.toList . coerce $ p

toInt :: Password -> Int
toInt x = sum $ zipWith (\d e -> d*base^e) (reverse l) [0..]
  where l = V.toList (coerce x)

next :: Password -> Password
next = (+ fromInteger 1)

instance Num Password where
  x + y = fromIntegral $ toInt x + toInt y
  fromInteger n = Password $ V.unfoldr f (fromInteger n, 7)
    where f (0, -1) = Nothing
          f (0, 0) = Just (0, (0, -1))
          f (0, n) = Just (0, (0, n-1))
          f (v, b) =
            let
              (w, r) = divMod v (base^b)
            in
              Just (w, (r, b-1))


upper = base ^ 8

az = ['a'..'z']
rev :: Map Char Int
rev = Map.fromList $ zip az [0..]

bad = mapMaybe (flip Map.lookup rev) "iol"

base = length az

-- passwords = generate az 8

valid :: Password -> Bool
valid p = noBad && straight && doubles
  where vp = coerce p :: Vector Int
        noBad = V.all (not . (`elem` bad)) vp
        straight = not . null . filter (\(a,b,c) -> a + 1 == b && b + 1 == c) . V.toList $ V.zip3 vp (V.tail vp) (V.tail (V.tail vp))
        doubles = doubles' 0 (V.toList vp)
          where doubles' _ [] = False
                doubles' _ [x] = False
                doubles' n (x:y:xs) =
                  case (x == y, n) of
                    (True, 1) -> True
                    (True, _) -> doubles' (n+1) xs
                    (False, _) -> doubles' n (y:xs)

nextValid = listToMaybe . take 1 . filter valid . drop 1 . iterate next

-- process input = take 1 . filter valid . dropWhile (/= input') $ passwords
--   where input' = fromString input

main = do
   let input = "cqjxjnds"
       nextp = nextValid $ fromStrRep input
       nextnextp = nextp >>= nextValid
   print nextp
   print nextnextp
