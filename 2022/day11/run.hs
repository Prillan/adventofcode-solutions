{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Control.Monad (replicateM_)
import Control.Monad.State (State, execState, runState, get, gets, modify, put)

import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord

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

-- TODO: REFACTOR AND FIX

type N = Integer

data Monkey = Monkey { mItems :: Seq N
                     , mOp    :: N -> N
                     , mPred  :: N
                     , mTrue  :: Int
                     , mFalse :: Int
                     }

instance Show Monkey where
  show = show . mItems
              
monkey0 = Monkey {
  mItems = [73, 77],
  mOp = (* 5),
  mPred = 11,
  mTrue = 6,
  mFalse = 5
}

monkey1 = Monkey {
  mItems = [57, 88, 80],
  mOp = (+ 5),
  mPred = 19,
  mTrue = 6,
  mFalse = 0
}

monkey2 = Monkey {
  mItems = [61, 81, 84, 69, 77, 88],
  mOp = (* 19),
  mPred = 5,
  mTrue = 3,
  mFalse = 1
}

monkey3 = Monkey {
  mItems = [78, 89, 71, 60, 81, 84, 87, 75],
  mOp = (+ 7),
  mPred = 3,
  mTrue = 1,
  mFalse = 0
}

monkey4 = Monkey {
  mItems = [60, 76, 90, 63, 86, 87, 89],
  mOp = (+ 2),
  mPred = 13,
  mTrue = 2,
  mFalse = 7
}

monkey5 = Monkey {
  mItems = [88],
  mOp = (+ 1),
  mPred = 17,
  mTrue = 4,
  mFalse = 7
}

monkey6 = Monkey {
  mItems = [84, 98, 78, 85],
  mOp = \x -> x * x,
  mPred = 7,
  mTrue = 5,
  mFalse = 4
}

monkey7 = Monkey {
  mItems = [98, 89, 78, 73, 71],
  mOp = (+ 4),
  mPred = 2,
  mTrue = 3,
  mFalse = 2
}

exampleMonkey0 = Monkey {
  mItems = [79, 98],
  mOp = (* 19),
  mPred = 23,
  mTrue = 2,
  mFalse = 3
  }

exampleMonkey1 = Monkey {
  mItems = [54, 65, 75, 74],
  mOp = (+ 6),
  mPred = 19,
  mTrue = 2,
  mFalse = 0
  }

exampleMonkey2 = Monkey {
  mItems = [79, 60, 97],
  mOp = \x -> x * x,
  mPred = 13,
  mTrue = 1,
  mFalse = 3
  }

exampleMonkey3 = Monkey {
  mItems = [74],
  mOp = (+ 3),
  mPred = 17,
  mTrue = 0,
  mFalse = 1
  }


starting :: Map Int Monkey
starting = Map.fromList [ (0, monkey0)
                        , (1, monkey1)
                        , (2, monkey2)
                        , (3, monkey3)
                        , (4, monkey4)
                        , (5, monkey5)
                        , (6, monkey6)
                        , (7, monkey7)
                        ]

exampleStarting :: Map Int Monkey
exampleStarting = Map.fromList [ (0, exampleMonkey0)
                               , (1, exampleMonkey1)
                               , (2, exampleMonkey2)
                               , (3, exampleMonkey3)
                               ]


type Monkeys = Map Int Monkey

data MState = MState { sMonkeys :: Monkeys
                     , sCounts  :: Counter Int
                     }
  deriving Show

initial = MState starting Map.empty
exampleInitial = MState exampleStarting Map.empty

step :: (N -> N) -> Int -> State MState ()
step wf mid = do
  m      <- gets $ (Map.! mid) . sMonkeys
  mtrue  <- gets $ mItems . (Map.! (mTrue m)) . sMonkeys
  mfalse <- gets $ mItems . (Map.! (mFalse m)) . sMonkeys

  let f its item = do
        let worry  = wf (mOp m item)
            target =
              if worry `mod` mPred m == 0
              then first
              else second
        pure $ target (Seq.:|> worry) its


  (mtrue', mfalse') <- foldlM f (mtrue, mfalse) (mItems m)
  modify $ \mstate ->
             mstate { sMonkeys =
                        Map.adjust (\x -> x { mItems = mtrue' }) (mTrue m)
                      . Map.adjust (\x -> x { mItems = mfalse' }) (mFalse m)
                      . Map.insert mid m { mItems = [] }
                      $ sMonkeys mstate,
                      sCounts =
                        Map.insertWith (+) mid (length (mItems m))
                        $ sCounts mstate
                    }

singleRound :: Int -> (N -> N) -> State MState ()
singleRound ms wf = mapM_ (step wf) ([0..ms] :: [Int])

runFor :: Int -> Int -> (N -> N) -> State MState ()
runFor ms steps wf =
  replicateM_ steps (singleRound ms wf)

activityLevel :: MState -> Int
activityLevel =
  product
  . take 2
  . sortOn negate
  . Map.elems
  . sCounts

part1 = activityLevel $ execState (runFor 7 20 (`div` 3)) initial
part2 =
  let modulus = product . map mPred $ Map.elems starting
  in activityLevel $ execState (runFor 7 10000 (`mod` modulus)) initial

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   print part1
   print part2
