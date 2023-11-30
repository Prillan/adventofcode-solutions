{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.List
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as VM

type Cup = Int
data Cups = Cups { cups :: !(Vector Int)
                 , currentCup :: !Cup
                 , lastCup :: !Cup }
  deriving (Show, Eq)

parseAll :: String -> [Cup]
parseAll = mapMaybe (readMaybe @Cup . pure)


setup :: Cup -> [Cup] -> Cups
setup maxCup cs =
  let l = maxCup - 1
      c = head cs - 1
      wrap | length cs == maxCup = (last shifted, head shifted)
           | otherwise           = (l, c)
      glue | length cs == maxCup = (last shifted, head shifted)
           | otherwise           = (last shifted, maximum shifted + 1)
      base = V.enumFromN 1 maxCup
      shifted = map (\x -> x - 1) cs
      firstPart =
        wrap:glue:zip shifted (drop 1 shifted)
      v = V.modify (flip update firstPart) base
  in Cups v c (fst wrap)


cupsToList :: Cups -> [Cup]
cupsToList c = cupsToList' (1 + currentCup c) c

cupsToList' :: Cup -> Cups -> [Cup]
cupsToList' root Cups {..} =
  let cs = map (+1) $ iterate' (cups V.!) (root - 1)
  in root:takeWhile (/= root) (drop 1 cs)

update :: MVector s Int -> [(Int, Int)] -> ST s ()
update mv toUpdate =
  forM_ toUpdate $ \(from, to) -> VM.write mv from to

readN :: Int -> Int -> MVector s Int -> ST s [Int]
readN n sidx v = go [] sidx n v
  where go acc   _ 0  _ = pure (sidx:reverse acc)
        go acc idx i cs = do
          idx' <- VM.read cs idx
          go (idx':acc) idx' (i - 1) cs

moveN :: Int -> Cup -> Cups -> ST s Cups
moveN n cupMax Cups {..} = V.thaw cups >>= go n currentCup lastCup
  where go :: Int -> Int -> Int -> MVector s Int -> ST s Cups
        go 0 curr l cs = Cups <$> V.freeze cs <*> pure curr <*> pure l
        go i curr l cs =
          readN 4 curr cs >>= \case
            _:c1:c2:c3:_ -> do
              let dests = filter (\x -> x /= c1 && x /= c2 && x /= c3)
                          . drop 1
                          $ iterate (\case 0 -> cupMax - 1
                                           x -> x - 1) curr
                  dest = head dests
              afterDest <- VM.read cs dest
              afterC3 <- VM.read cs c3
              let toInsert :: [(Int, Int)]
                  toInsert
                    | dest == l =
                      [ (dest, c1)
                      , (c3, curr)
                      , (curr, afterC3) ]
                    | otherwise =
                      [ (dest, c1)
                      , (c3, afterDest)
                      , (l, curr)
                      , (curr, afterC3) ]
                  last' = curr
                  curr' = afterC3
              update cs toInsert
              go (i - 1) curr' last' cs
            _ -> error "unreachable"

part1 :: [Cup] -> String
part1 input =
  let final = runST $ moveN 100
                            (maximum input)
                            (setup (length input) input)
      result = concatMap show
               . drop 1
               $ cupsToList' 1 final
  in result

part2 :: [Cup] -> Int
part2 input =
  let end = 1_000_000
      final = runST $ moveN 10_000_000
                            end
                            (setup end input)
      result = take 2
               . drop 1
               $ cupsToList' 1 final
  in product result

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   putStrLn (part1 input)
   print (part2 input)
