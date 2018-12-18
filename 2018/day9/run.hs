{-# LANGUAGE RecordWildCards #-}
import Control.Monad (forM_)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq(..), (|>), (<|))
import qualified Data.Sequence as Seq

type Counter a = Map a Int

data Game = Game { players :: Int
                 , turn    :: Int
                 , board   :: Seq Int
                 , scores  :: Counter Int }
  deriving Show

emptyGame :: Int -> Game
emptyGame p = Game { players = p
                   , turn = 0
                   , board = Seq.empty
                   , scores = Map.empty }

highscore :: Game -> Int
highscore = maximum . Map.elems . scores

rotateLeft :: Int -> Seq Int -> Seq Int
rotateLeft _ Empty     = Seq.empty
rotateLeft 0 s         = s
rotateLeft n (x :<| s) = rotateLeft (n-1) (s |> x)

rotateRight :: Int -> Seq Int -> Seq Int
rotateRight _ Empty     = Seq.empty
rotateRight 0 s         = s
rotateRight n (s :|> x) = rotateRight (n-1) (x <| s)

addScore :: Counter Int -> Int -> Int -> Counter Int
addScore s p n = Map.unionWith (+) s (Map.singleton p n)

stepGame :: Game -> Int -> Game
stepGame g@Game {..} i
  | i > 0 && i `mod` 23 == 0 =
    let (x :<| board') = rotateRight 7 board
    in g { scores = addScore scores turn (i + x)
         , turn   = (turn + 1) `mod` players
         , board  = board' }
  | otherwise = g { turn  = (turn + 1) `mod` players
                  , board = i <| rotateLeft 2 board }

solve :: Int -> Int -> Int
solve players lastMarble =
  highscore $ foldl' stepGame (emptyGame players) [0..lastMarble]

testCases :: [(Int, Int, Int)]
testCases =
  [ (10, 1618, 8317)
  , (13, 7999, 146373)
  , (17, 1104, 2764)
  , (21, 6111, 54718)
  , (30, 5807, 37305) ]

test :: IO ()
test = forM_ testCases $ \(ps, lm, r) -> do
  let result = solve ps lm
  if result == r
    then putStrLn $ show ps ++ ", " ++ show lm ++ " succeeded!"
    else putStrLn $ show ps
                    ++ ", "
                    ++ show lm
                    ++ " fail! Expected "
                    ++ show r
                    ++ " but got "
                    ++ show result

main :: IO ()
main = do
  -- 427 players; last marble is worth 70723 points
  print (solve 427 70723)
  print (solve 427 7072300)
