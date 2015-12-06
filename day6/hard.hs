import Data.Maybe
import Text.Parsec

import Data.List (foldl')
import Data.Vector (Vector)
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

flipSwitch :: Int -> LightState -> LightState
flipSwitch n | even n    = id
             | otherwise = toggle

theseInstructions :: Position -> [Instruction] -> [Instruction]
theseInstructions p = filter (inRange p)

finalState :: [Instruction] -> Position -> LightState
finalState = finalState' 0
  where finalState' n [] _ = flipSwitch n Off
        finalState' n (i@(I _ _ (Turn t)):is) p
          | inRange p i = flipSwitch n t
          | otherwise = finalState' n is p
        finalState' n (i@(I _ _ Toggle):is) p
          | inRange p i = finalState' (n+1) is p
          | otherwise = finalState' n is p

process input = length
              . filter (==On)
              . map (finalState instructions)
              $ [(P x y) | x <- [0..999], y <- [0..999]]
  where instructions = reverse . mapMaybe readInstruction . lines $ input

main = do
   input <- readFile "input.txt"
   print (process input)
