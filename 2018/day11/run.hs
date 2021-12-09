{-# LANGUAGE BangPatterns #-}
import Data.Ord (comparing)
import Data.List (maximumBy, sortBy, transpose)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

level :: Int -> Int -> Int -> Int
level serial x y =
  let rackId = x + 10
  in
    ((((rackId * y + serial) * rackId) `div` 100) `mod` 10) - 5

type Grid = Vector Int

initGrid :: Int -> [[Int]]
initGrid serial =
  [ [ level serial x y | x <- [1..300] ] | y <- [1..300] ]

data Square = Square { left, top, power, size :: !Int }
  deriving (Show, Eq)

flat :: [[Int]] -> Vector Int
flat = V.fromList . concat

lkp :: Grid -> Int -> Int -> Int -> Int
lkp v n x y = v V.! (n * y + x)

squares' :: Int -> Grid -> Int -> [Square]
squares' n grid s = do
  t <- [0..n - s]
  l <- [0..n - s]
  let b = t + s
      r = l + s

      -- Given a matrix (M) with submatrices A, B, C, D
      -- / A B \
      -- \ C D /
      --
      -- we have sum D = sum M - (sum A + sum B + sum C)
      -- and we can get sum A = G_(l, t)
      --                sum B = G_(l+s, t)
      -- etc. to compute D without summing all elements

      !sM  = lkp grid (n + 1) (l + s) (t + s)
      !sAC = lkp grid (n + 1) l       (t + s)
      !sAB = lkp grid (n + 1) (l + s) t
      !sA  = lkp grid (n + 1) l       t

  pure $ Square { left  = l + 1
                , top   = t + 1
                , power = sM + sA - sAB - sAC
                , size  = s }


example :: [[Int]]
example =
  [ [-2, -4,  4,  4,  4]
  , [-4,  4,  4,  4, -5]
  , [ 4,  3,  3,  4, -4]
  , [ 1,  1,  2,  4, -3]
  , [-1,  0,  2, -5, -2]
  ]

-- Computes rolling sums in both dimensions
sums :: [[Int]] -> [[Int]]
sums =
  transpose
  . map (scanl (+) 0)
  . transpose
  . map (scanl (+) 0)

part1 :: Int -> Square
part1 = maximumBy (comparing power)
        . flip (squares' 300) 3
        . flat
        . sums
        . initGrid

part2 :: Int -> Square
part2 serial =
  let !grid = flat $ sums (initGrid serial)
  in maximumBy (comparing power)
     . concatMap (squares' 300 grid)
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
  putStrLn . result1 . part1 $ input
  putStrLn . result2 . part2 $ input
