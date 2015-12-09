import           Data.Bits
import           Data.Functor.Identity
import           Data.Map (Map)
import qualified Data.Map.Lazy as Map
import           Data.Maybe (fromJust, mapMaybe)
import           Data.Word
import           Text.Parsec

type W = Word16

newtype Register = Register String
  deriving (Show, Eq, Ord)
data Value = Wire Register | Number W
  deriving Show
data Instruction = I Command Register
  deriving Show
data Command = SET Value
             | AND Value Value
             | OR Value Value
             | LSHIFT Value Int
             | RSHIFT Value Int
             | NOT Value
  deriving Show

instance Eq a => Eq (b -> a) where
  _ == _ = True
instance Bits a => Bits (b -> a) where
  f .&. g   = \x -> f x .&. g x
  f .|. g   = \x -> f x .|. g x
  f `xor` g = \x -> f x `xor` g x
  complement = (complement.)
  shiftL f n = \x -> shiftL (f x) n
  shiftR f n = \x -> shiftR (f x) n

number :: Num a => ParsecT String u Identity a
number = fromInteger . read <$> many1 digit

register = Register <$> many1 alphaNum

val = (Number <$> number) <|> (Wire <$> register)

command =  try (AND <$> val <*> (string " AND " *> val))
       <|> try (OR <$> val <*> (string " OR " *> val))
       <|> try (NOT <$> (string "NOT " *> val))
       <|> try (LSHIFT <$> val <*> (string " LSHIFT " *> number))
       <|> try (RSHIFT <$> val <*> (string " RSHIFT " *> number))
       <|> try (SET <$> val)

instruction = I <$> command <*> (string " -> " *> register)

readInstruction = parse instruction ""

lkup x y = fromJust $ Map.lookup x y

valueToFun :: Value -> Map Register W -> W
valueToFun (Wire a) = lkup a
valueToFun (Number v) = const v

toFunction :: Instruction -> (Register, Map Register W -> W)
toFunction (I cmd o) = (o, case cmd of
                             AND x y -> wtf x .&. wtf y
                             OR x y -> wtf x .|. wtf y
                             NOT x -> complement (wtf x)
                             SET x -> wtf x
                             LSHIFT x n -> shiftL (wtf x) n
                             RSHIFT x n -> shiftR (wtf x) n)
  where wtf = valueToFun

mapEither f = mapMaybe (g . f)
  where g (Left _) = Nothing
        g (Right x) = Just x

-- The spreadsheet evaluator
loeb x = fmap (\a -> a (loeb x)) x

process input = lkup (Register "a") $ loeb ssheet
  where ssheet = Map.fromList
               . map toFunction
               . mapEither readInstruction . lines $ input

main = do
   input <- readFile "input.txt"
   print (process input)
