{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified Data.ByteString.Lazy as B

nums = go
  where go = \case
          Object o -> concatMap go o
          Array a  -> concatMap go a
          Number n -> pure n
          _        -> []

part1 :: Value -> Int
part1 = truncate . sum . nums

nums' = go
  where go = \case
          Object o | String "red" `elem` o -> []
                   | otherwise             -> concatMap go o
          Array a  -> concatMap go a
          Number n -> pure n
          _        -> []

part2 :: Value -> Int
part2 = truncate . sum . nums'

main = do
  Just input <- decode <$> B.readFile "input.txt"
  print (part1 input)
  print (part2 input)
