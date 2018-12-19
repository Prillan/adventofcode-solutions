{-# LANGUAGE BangPatterns #-}
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Vector (Vector)
import qualified Data.Vector as V

level :: Int -> Int -> Int -> Int
level serial x y =
  let rackId = x + 10
  in
    ((((rackId * y + serial) * rackId) `div` 100) `mod` 10) - 5

type Grid = Vector (Vector Int)

initGrid :: Int -> Grid
initGrid serial =
  V.fromList [ V.fromList [ level serial x y | y <- [1..300] ] | x <- [1..300] ]

square :: Int -> Int -> Int -> Grid -> Vector Int
square s t l = V.concatMap (V.slice t s) . V.slice l s

data Square = Square { left, top, power, size :: !Int }
  deriving Show

squares :: Grid -> Int -> [Square]
squares grid s = do
  l <- [1..300-s+1]
  t <- [1..300-s+1]
  pure $ Square { left  = l
                , top   = t
                , power = V.sum . square s (t-1) (l-1) $ grid
                , size  = s }

part1 :: Int -> Square
part1 = maximumBy (comparing power)
        . flip squares 3
        . initGrid

part2 :: Int -> Square
part2 serial =
  let grid = initGrid serial
  in maximumBy (comparing power)
     . concatMap (squares grid)
     $ [1..300]

result1 :: Square -> String
result1 s = show (left s) ++ "," ++ show (top s)

result2 :: Square -> String
result2 s = show (left s) ++ ","
            ++ show (top s) ++ ","
            ++ show (size s)

main :: IO ()
main = do
  let input = 5535

  let r1 = part1 input
  print r1
  putStrLn (result1 r1)
  let r2 = part2 input
  print r2
  putStrLn (result2 r2)
