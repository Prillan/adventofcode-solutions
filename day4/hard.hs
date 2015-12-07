{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import Crypto.Hash
import Data.Semigroup ((<>))

privateKey :: B.ByteString
privateKey = "yzbqklnj"

candidates = map (\i -> privateKey <> (B.pack $ show i)) $ [(1 :: Integer)..]

process = filter (all (== '0') . take 6 . show . snd)
        . map (\c -> (c, hash c :: Digest MD5)) $ candidates

main = do
   print (head process)
