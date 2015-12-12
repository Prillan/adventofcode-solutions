{-# LANGUAGE OverloadedStrings #-}
import Control.Monad ((>=>))
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Foldable
import Data.Scientific

nums :: Value -> [Scientific]
nums (Object o) = foldl' (\acc v -> nums v ++ acc) [] o
nums (Array a) = foldl' (\acc v -> nums v ++ acc) [] a
nums (Number n) = pure n
nums _ = []

ints = map truncate

process = decode >=> fmap (sum . ints . nums)

main = do
   input <- B.readFile "input.json"
   print (process input)
