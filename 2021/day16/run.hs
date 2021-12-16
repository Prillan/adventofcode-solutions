{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Control.Applicative
import Control.Monad (replicateM)
import Data.Bifunctor
import Data.Bits (FiniteBits, Bits, finiteBitSize)
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord
import Numeric (readHex)
import Data.Word
import Data.List.Split (chunksOf)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

newtype Get a = Get { runGet :: [Bool] -> Maybe (a, [Bool]) }
  deriving Functor

instance Applicative Get where
  pure x = Get $ \v -> Just (x, v)
  mf <*> mx = Get $ \v -> do
    (f, v') <- runGet mf v
    (x, v'') <- runGet mx v'
    pure (f x, v'')

instance Monad Get where
  mx >>= f = Get $ \v -> do
    (x, v') <- runGet mx v
    runGet (f x) v'

instance Alternative Get where
  empty = Get $ const Nothing
  mx <|> my = try mx >>= \case Just v  -> pure v
                               Nothing -> my

bits :: forall a. (Num a, FiniteBits a) => Get a
bits = bitsN (finiteBitSize (0 :: a))

bitsN :: (Num a, Bits a) => Int -> Get a
bitsN n = Get $ \v ->
  case splitAt n v of
    ([], _) -> Nothing
    (h, t) | length h == n -> Just (bitsFromBools h, t)
           | otherwise     -> Nothing

integer :: Get Integer
integer = Get $ \v -> Just (bitsFromBools v, [])

bit :: Get Bool
bit = Get $ \case (x:xs) -> Just (x, xs)
                  _      -> Nothing

variable :: Get a -> Get a
variable g = Get $ \v -> do
    (x, v') <- runGet go v
    (y, _) <- runGet g x
    pure (y, v')
  where go = bit >>= \case False -> replicateM 4 bit
                           True  -> (++) <$> replicateM 4 bit <*> go

restrict :: Int -> Get a -> Get a
restrict n g = Get $ \v -> 
  case splitAt n v of
    ([], _) -> Nothing
    (h, t) -> do
      (x, _) <- runGet g h
      pure (x, t)

try :: Get a -> Get (Maybe a)
try g = Get $ \v ->
  case runGet g v of
    Just (x, v') -> Just (Just x, v')
    Nothing      -> Just (Nothing, v)

decode :: [Word8] -> Get a -> Maybe a
decode x g = fst <$> runGet g (concatMap (boolsFromBits 8) x)

parseAll :: String -> [Word8]
parseAll = concatMap (fmap fst . readHex @Word8) . chunksOf 2

data Packet = Packet { version :: Int
                     , typeId  :: Int
                     , content :: Content
                     }

data Content = Literal Integer | Operator (Integer -> Integer -> Integer) [Packet]

gt :: (Ord a, Num a) => a -> a -> a
gt x y | x > y     = 1
       | otherwise = 0

lt :: (Ord a, Num a) => a -> a -> a
lt x y | x < y     = 1
       | otherwise = 0

eq :: (Eq a, Num a) => a -> a -> a
eq x y | x == y    = 1
       | otherwise = 0

packet :: Get Packet
packet = do
  version <- bitsN @Int 3
  typeId  <- bitsN @Int 3
  content <- case typeId of
               4 -> Literal <$> literal
               0 -> Operator (+) <$> subpackets
               1 -> Operator (*) <$> subpackets
               2 -> Operator min <$> subpackets
               3 -> Operator max <$> subpackets
               5 -> Operator gt  <$> subpackets
               6 -> Operator lt  <$> subpackets
               7 -> Operator eq  <$> subpackets
  pure $ Packet version typeId content

literal :: Get Integer
literal = variable integer

subpackets :: Get [Packet]
subpackets =
  bit >>= \case
    False -> bitsN @Int 15 >>= \n -> restrict n (some packet)
    True  -> bitsN @Int 11 >>= \n -> replicateM n packet

versions :: Packet -> Int
versions = go
  where go Packet {..} =
          version + case content of
                      Literal _ -> 0
                      Operator _ ps -> sum $ map go ps

value :: Packet -> Integer
value = go . content
  where go = \case
          Literal v -> v
          Operator f ps -> foldl1' f (map (go . content) ps)

part1 input =
  let Just p = decode input packet
  in versions p
part2 input =
  let Just p = decode input packet
  in value p

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
