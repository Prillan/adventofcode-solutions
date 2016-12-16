import           Data.ByteString.Char8 (ByteString, pack, unpack)
import           Data.List (isPrefixOf)
import           Crypto.Hash
import           Iterated (iterated)

import System.IO (stdout, hSetBuffering, BufferMode(..))

md5 :: ByteString -> Digest MD5
md5 = hash

md5' :: ByteString -> ByteString
md5' = pack . show . md5

data Queue a = Queue [a] [a]
newQueue = Queue [] []
fromList = flip Queue []
toList (Queue front back) = front ++ reverse back
enq (Queue xs ys) y = Queue xs (y:ys)
deq (Queue [] []) = Nothing
deq (Queue (x:xs) ys) = Just (x, Queue xs ys)
deq (Queue [] ys) = deq (Queue (reverse ys) [])

firstTriple :: Eq a => [a] -> Maybe a
firstTriple (x:y:z:rest)
  | x == y && y == z = Just x
  | x /= y && y == z = firstTriple (y:z:rest)
  | otherwise        = firstTriple (z:rest)
firstTriple _ = Nothing

has5Tuple :: Eq a => a -> [a] -> Bool
has5Tuple e (x0:x1:x2:x3:x4:rest)
  | e == x0 && x0 == x1 && x1 == x2 && x2 == x3 && x3 == x4 = True
  | e == x1 && x1 == x2 && x2 == x3 && x3 == x4 = has5Tuple e (x1:x2:x3:x4:rest)
  | e == x2 && x2 == x3 && x3 == x4 = has5Tuple e (x2:x3:x4:rest)
  | e == x3 && x3 == x4 = has5Tuple e (x3:x4:rest)
  | e == x4 = has5Tuple e (x4:rest)
  | otherwise = has5Tuple e rest
has5Tuple _ _ = False

keys :: (ByteString -> ByteString) -> [(Int, Char, String)]
keys f =
  let hashes = map (unpack . f . pack . (part1Input ++) . show) [0..]
      initial = (fromList $ take 1000 hashes, drop 1000 hashes)
      go (q, (h:hs)) n =
        let Just (c, q') = deq (enq q h)
        in case firstTriple c of
             Just x  ->
               if any (has5Tuple x) (toList q')
                 then (n, x, c):(go (q', hs) (n+1))
                 else go (q', hs) (n+1)
             Nothing -> go (q', hs) (n+1)
  in go initial 0

part1Input = "ahsbgdzn"
part1 = keys md5'
part2 = keys iterated

main = do
  mapM_ print $ zip [1..64] part1
  mapM_ print $ zip [1..64] part2
