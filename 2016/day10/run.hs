{-# LANGUAGE TupleSections #-}
import           Data.Bifunctor
import           Data.List (sort)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Text.Megaparsec hiding (State, empty)
import           Text.Megaparsec.Char
import           Data.Void (Void)

unsafeRight (Right x) = x

type BotId = Int
type OutputId = Int
data Target = B BotId | O OutputId
type Logic = (HashMap BotId [Int], HashMap OutputId Int)

empty :: Logic
empty = (HashMap.empty, HashMap.empty)

num = read <$> some digitChar

insertBot :: Int -> BotId -> Logic -> Logic
insertBot val bid = first (HashMap.insertWith (++) bid [val])

insertOut :: Int -> OutputId -> Logic -> Logic
insertOut val oid = second (HashMap.insert oid val)

insertTarget :: Int -> Target -> Logic -> Logic
insertTarget val t =
  case t of
    O oid -> insertOut val oid
    B bid -> insertBot val bid

insert val bid = Just . insertBot val bid

move :: BotId -> Target -> Target -> Logic -> Maybe Logic
move bid lowTo highTo logic = do
  [low, high] <- sort <$> HashMap.lookup bid (fst logic)
  pure . insertTarget low lowTo . insertTarget high highTo $ logic

type Inst = Logic -> Maybe Logic

insertP = insert <$> (string "value " *> num)
                 <*> (string " goes to bot " *> num)
moveP = move <$> (string "bot " *> num)
             <*> (string " gives low to " *> targetP)
             <*> (string " and high to " *> targetP)
targetP = O <$> (string "output " *> num)
      <|> B <$> (string "bot " *> num)

inst :: Parsec Void String Inst
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
  . HashMap.toList
  . fst
  . eval
part2 :: [Inst] -> Int
part2 =
  product
  . map snd
  . filter ((`elem` [0,1,2]) . fst)
  . HashMap.toList
  . snd
  . eval

loeb x = fmap (\a -> a (loeb x)) x

main = do
  input <- parseAll <$> getContents
  print (part1 input)
  print (part2 input)
