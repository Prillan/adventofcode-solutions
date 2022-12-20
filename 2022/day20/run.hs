{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import Data.Foldable
import Data.List

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Control.Monad.ST

import Data.Ix (index, rangeSize)

import Data.Vector.Unboxed (MVector, Vector)
import qualified  Data.Vector.Unboxed as V
import qualified  Data.Vector.Unboxed.Mutable as MV

type N = Int
type Elem = (N, N)
type Bounds = ((N, N), (N, N))

build :: [N] -> [(N, Int)]
build nums = go HashMap.empty nums
  where go _ [] = []
        go seen (n:ns) =
          case HashMap.lookup n seen of
            Just i  -> (n, i + 1):go (HashMap.insert n (i + 1) seen) ns
            Nothing -> (n,     0):go (HashMap.insert n       1 seen) ns

parseAll :: String -> [N]
parseAll = map (read @N) . lines

setup :: [N] -> (Bounds, Vector Elem, Vector Elem, [Elem])
setup nums =
  let low = minimum nums
      high = maximum nums
      indexed = build nums
      recur = maximum $ map snd indexed
      bounds = ((low, 0), (high, recur))
      len = rangeSize bounds

      forward = V.create do
        v <- MV.new len
        _ <- forM_ (zip indexed (drop 1 indexed ++ take 1 indexed)) \(from, to) ->
          link bounds v from to
        pure v

      backward = V.create do
        v <- MV.new len
        _ <- forM_ (zip indexed (drop 1 indexed ++ take 1 indexed)) \(from, to) ->
          link bounds v to from
        pure v
  in
    (bounds, forward, backward, indexed)

link :: Bounds -> MVector s Elem -> Elem -> Elem -> ST s ()
link bounds v from to =
  MV.write v (index bounds from) to
{-# INLINE link #-}

forwardToList :: Bounds -> Vector Elem -> [Elem]
forwardToList bounds forward = (0, 0):go (forward V.! index bounds (0, 0))
  where go (0, 0) = []
        go !i = i:go (forward V.! index bounds i)

mfollow :: Bounds -> MVector s Elem -> Elem -> N -> ST s (N, N)
mfollow bounds v start = go start
  where go !x 0 = pure x
        go !x !n = do
          !y <- MV.read v (index bounds x)
          go y (n  -1)
{-# INLINE mfollow #-}

move :: (N -> N) -> Int -> Bounds -> (MVector s Elem, MVector s Elem) -> Elem -> ST s ()
move modifier len bounds (!forward, !backward) i@(n, _) =
  case (modifier n) `mod` (len - 1) of
    n'
      | n' == 0 -> pure ()
      | n' == 1
        -> do
          --- w -> i -> t -> z
          --- w -> t -> i -> z
          !w <- MV.read backward (index bounds i)
          !t <- MV.read forward  (index bounds i)
          !z <- MV.read forward  (index bounds t)

          _ <- link bounds backward t w
          _ <- link bounds backward i t
          _ <- link bounds backward z i
          _ <- link bounds forward w t
          _ <- link bounds forward t i
          _ <- link bounds forward i z
          pure ()
      | n' == -1
        -> do
          --- w -> t -> i -> z
          --- w -> i -> t -> z
          !t <- MV.read backward (index bounds i)
          !z <- MV.read forward  (index bounds i)
          !w <- MV.read backward (index bounds t)

          _ <- link bounds backward i w
          _ <- link bounds backward t i
          _ <- link bounds backward z t
          _ <- link bounds forward w i
          _ <- link bounds forward i t
          _ <- link bounds forward t z
          pure ()
      | n' > 1  -> do
          --- w -> i -> x -> ... -> t -> z
          --- w -> x -> ...      -> t -> i -> z
          !w <- MV.read backward (index bounds i)
          !x <- MV.read forward  (index bounds i)
          !t <- mfollow bounds forward i n'
          !z <- MV.read forward  (index bounds t)

          _ <- link bounds backward x w
          _ <- link bounds backward i t
          _ <- link bounds backward z i
          _ <- link bounds forward w x
          _ <- link bounds forward t i
          _ <- link bounds forward i z
          pure ()

      | otherwise -> do -- i < -1
          --- x -> t ->      ... -> y -> i -> z
          --- x -> i -> t -> ... -> y -> z
          !y <- MV.read backward (index bounds i)
          !z <- MV.read forward  (index bounds i)
          !t <- mfollow bounds backward i (-n')
          !x <- MV.read backward (index bounds t)

          _ <- link bounds backward z y
          _ <- link bounds backward t i
          _ <- link bounds backward i x
          _ <- link bounds forward x i
          _ <- link bounds forward i t
          _ <- link bounds forward y z

          pure ()


handle :: (N -> N)
       -> Bounds
       -> (Vector Elem, Vector Elem)
       -> [Elem]
       -> (Vector Elem, Vector Elem)
handle modifier bounds (f, b) indexed = runST do
  let len = length indexed
  forward  <- V.thaw f
  backward <- V.thaw b
  forM_ indexed \n ->
    move modifier len bounds (forward, backward) n

  (,) <$> V.freeze forward <*> V.freeze backward

part1 :: [N] -> N
part1 input =
  let (bounds, forward, backward, indexed) = setup input
      (!f, _) = handle id bounds (forward, backward) indexed
      len = length indexed
      fl = map fst $ forwardToList bounds f
  in
    sum [ fl !! (1000 `mod` len)
        , fl !! (2000 `mod` len)
        , fl !! (3000 `mod` len)
        ]

decryptionKey :: N
decryptionKey = 811589153

--- UPDATE INDEXING TO HANDLE DECRYPTION KEY
part2 :: [N] -> N
part2 input =
  let (bounds, forward, backward, indexed) = setup input
      len = length indexed
      single (f, b) = handle (* decryptionKey) bounds (f, b) indexed
      xs = zip @N [0..]
           . map (map (*decryptionKey) . map fst . forwardToList bounds)
           . map fst
           $ scanl' (\fb _ -> single fb) (forward, backward) [1..10::Int]

      (_, fl) = last xs
  in
    sum [ fl !! (1000 `mod` len)
        , fl !! (2000 `mod` len)
        , fl !! (3000 `mod` len)
        ]

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
