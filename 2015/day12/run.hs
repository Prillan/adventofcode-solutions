{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Semigroup (Sum(..))

part1 :: Value -> Int
part1 = getSum . go
  where go = \case
          Object o -> foldMap go o
          Array a  -> foldMap go a
          Number n -> Sum $ truncate n
          _        -> 0

part2 :: Value -> Int
part2 = getSum . go
  where go = \case
          Object o | String "red" `elem` o -> 0
                   | otherwise             -> foldMap go o
          Array a  -> foldMap go a
          Number n -> Sum $ truncate n
          _        -> 0

main = do
  Just input <- decode <$> B.readFile "input.txt"
  print (part1 input)
  print (part2 input)
