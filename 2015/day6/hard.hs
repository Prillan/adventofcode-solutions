import Data.Maybe
import Text.Parsec

import Data.Foldable
import Data.List (foldl')
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V

data Position = P { x :: Int, y :: Int}
  deriving (Eq, Show)

instance Ord Position where
  compare p1 p2 = compare (x p1, y p1) (x p2, y p2)

data Command = Turn LightState | Toggle
  deriving Show
data Instruction = I Position Position Command
  deriving Show

data LightState = On | Off
  deriving (Eq, Show)

toggle :: LightState -> LightState
toggle On = Off
toggle Off = On

inRange :: Position -> Instruction -> Bool
inRange (P px py) (I start stop _) = x start <= px && px <= x stop
                                  && y start <= py && py <= y stop

number = read <$> many1 digit

position = P <$> number <*> (char ',' *> number)
command = do
  cmd <- try (string "turn on") <|> try (string "turn off") <|> string "toggle"
  case cmd of
    "turn on" -> pure (Turn On)
    "turn off" -> pure (Turn Off)
    "toggle" -> pure Toggle
instruction = do
  cmd <- command
  _ <- char ' '
  start <- position
  _ <- string " through "
  end <- position
  pure $ I start end cmd

toMaybe (Left _) = Nothing
toMaybe (Right x) = Just x

readInstruction = toMaybe . parse instruction ""

toUpdate (P sx sy) (P ex ey) = [x+y*1000 | x <- [sx..ex], y <- [sy..ey]]

execute :: Command -> Int -> Int
execute (Turn On) = (+1)
execute (Turn Off) = max 0 . (+(-1))
execute Toggle = (+2)

unsafeModify v f i = M.unsafeRead v i >>= M.unsafeWrite v i . f

process input = do
  let instructions = mapMaybe readInstruction . lines $ input
  v <- M.new (1000*1000)
  M.set v 0
  forM_ instructions $ \(I start end cmd) -> do
    putStrLn (show start ++ ", " ++ show end ++ ", " ++ show cmd)
    forM_ (toUpdate start end) $ \i -> unsafeModify v (execute cmd) i
  V.freeze v


main = do
   input <- readFile "input.txt"
   output <- process input
   print (sum output)
