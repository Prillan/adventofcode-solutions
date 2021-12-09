{-# LANGUAGE TypeApplications #-}
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type Arg = Int

data Inst = Acc Arg
          | Jmp Arg
          | Nop Arg
  deriving (Show, Eq)

instance Read Inst where
  readsPrec _ x = do
    let (inst, ' ':val) = break (== ' ') x
    (arg, rest) <- reads . dropWhile (== '+') $ val
    let inst' = case inst of
                  "acc" -> Acc
                  "jmp" -> Jmp
                  "nop" -> Nop
    pure (inst' arg, rest)

parseAll =
  map (read @Inst)
  . lines


execStep :: (Int, Int, [Inst]) -> Maybe (Int, Int, [Inst])
execStep (pc, acc, prog) =
  case drop pc prog of
    (Acc v:_) -> Just (pc + 1, acc + v, prog)
    (Jmp v:_) -> Just (pc + v, acc, prog)
    (Nop _:_) -> Just (pc + 1, acc, prog)
    [] -> Nothing

exec prog = iterate (>>= execStep) $ Just (0, 0, prog)

execUntilLoop input =
  let execution = mapMaybe id
                  . takeWhile isJust
                  $ exec input
  in listToMaybe
     . filter fst
     . map (\(v, (pc, acc, _)) -> (pc `Set.member` v, acc))
     $ zip (visited execution) execution

visited = scanl f Set.empty
  where f pcs (pc, _, _) = Set.insert pc pcs

loops = isJust . execUntilLoop

singleMap f [] = []
singleMap f (x:xs) = (f x:xs):map (x:) (singleMap f xs)

mutate (Acc _) = Nothing
mutate (Jmp v) = Just (Nop v)
mutate (Nop v) = Just (Jmp v)

part1 = maybe undefined snd . execUntilLoop
part2 input =
  let candidates = mapMaybe sequence
                   . singleMap (>>= mutate)
                   . map pure
                   $ input
      correct = head . filter (not . loops) $ candidates
      Just (_, acc, _) = last
                         . takeWhile isJust
                         $ exec correct
  in acc


main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
