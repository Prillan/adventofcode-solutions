{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import Crypto.Hash
import Data.Semigroup ((<>))

privateKey :: String
privateKey = "yzbqklnj"

hashZeroes n i =
  let c = B.pack $ privateKey <> show i
      h = hash c :: Digest MD5
  in all (== '0') . take n . show $ h

part1 = filter (hashZeroes 5) [0..]
part2 = filter (hashZeroes 6) part1

main = do
  print (head part1)
  print (head part2)
