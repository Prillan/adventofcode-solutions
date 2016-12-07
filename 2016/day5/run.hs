import           Data.ByteString.Lazy.Char8 (ByteString, pack)
import           Data.List (isPrefixOf)
import           Crypto.Hash

import System.IO (stdout, hSetBuffering, BufferMode(..))

md5 :: ByteString -> Digest MD5
md5 = hashlazy

doorHash :: String -> Integer -> String
doorHash d n = show . md5 . pack $ d ++ show n

part1 = concat
  . take 8
  . map (take 1 . drop 5)
  . filter ("00000" `isPrefixOf`)
  . map (doorHash input) $ [0..]

tpl [x, y] = (x, y)

part2 = map (\i -> snd . head $ filter ((== i) . fst) candidates) "01234567"
  where candidates = filter ((`elem` "01234567") . fst)
                   . map (tpl . take 2 . drop 5)
                   . filter ("00000" `isPrefixOf`)
                   . map (doorHash input) $ [0..]

input :: String
input = "reyedfim"

main = do
  hSetBuffering stdout NoBuffering
  print part1
  print part2
