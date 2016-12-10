{-# LANGUAGE TupleSections #-}
import           Data.Bifunctor
import           Data.List (sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.Megaparsec hiding (State)

unsafeRight (Right x) = x

type BotId = Int
type OutputId = Int
data Target = B BotId | O OutputId
type Logic = (Map BotId [Int], Map OutputId Int)

empty :: Logic
empty = (Map.empty, Map.empty)

num = read <$> some digitChar

insertBot :: Int -> BotId -> Logic -> Logic
insertBot val bid = first (Map.insertWith (++) bid [val])

insertOut :: Int -> OutputId -> Logic -> Logic
insertOut val oid = second (Map.insert oid val)

insertTarget :: Int -> Target -> Logic -> Logic
insertTarget val t =
  case t of
    O oid -> insertOut val oid
    B bid -> insertBot val bid

insert val bid = Just . insertBot val bid

move :: BotId -> Target -> Target -> Logic -> Maybe Logic
move bid lowTo highTo logic = do
  [low, high] <- sort <$> Map.lookup bid (fst logic)
  pure . insertTarget low lowTo . insertTarget high highTo $ logic

type Inst = Logic -> Maybe Logic

insertP = insert <$> (string "value " *> num)
                 <*> (string " goes to bot " *> num)
moveP = move <$> (string "bot " *> num)
             <*> (string " gives low to " *> targetP)
             <*> (string " and high to " *> targetP)
targetP = O <$> (string "output " *> num)
      <|> B <$> (string "bot " *> num)

inst :: Parsec Dec String Inst
inst = insertP <|> moveP

parseAll = map unsafeRight .
  map (parse inst "") . lines

eval :: [Inst] -> Logic
eval = snd . head . dropWhile (not.null.fst) . iterate eval' . (,empty)
  where eval' :: ([Inst], Logic) -> ([Inst], Logic)
        eval' (inst, l) = first reverse $ foldl step ([], l) inst

        step :: ([Inst], Logic) -> Inst -> ([Inst], Logic)
        step (rem, l) instr =
          case instr l of
            Just l' -> (rem, l')
            Nothing -> (instr:rem, l)

part1 :: [Inst] -> BotId
part1 =
  fst . head
  . filter ((== [17, 61]) . snd)
  . map (second sort)
  . Map.toList
  . fst
  . eval
part2 :: [Inst] -> Int
part2 =
  product
  . map snd
  . filter ((`elem` [0,1,2]) . fst)
  . Map.toList
  . snd
  . eval

loeb x = fmap (\a -> a (loeb x)) x

main = do
  input <- parseAll <$> getContents
  print (part1 input)
  print (part2 input)
