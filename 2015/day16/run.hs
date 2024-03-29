import Data.Aeson
import Text.Parsec

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data Sue = Sue { sueIndex :: Int
               , sueProps :: HashMap String Int}
  deriving Show

unsafeRight (Right x) = x

readInt = (read :: String -> Int) <$> many1 digit

sue = Sue <$> (string "Sue " *> (readInt <* string ": "))
          <*> (HashMap.fromList <$> sepBy property (string ", "))
  where property = (,) <$> many1 alphaNum <*> (string ": " *> readInt)

parseAll = map unsafeRight . map (parse sue "") . lines

machineOutput = Sue {
  sueIndex = -1,
  sueProps = HashMap.fromList [("akitas",0),("cars",2),("cats",7),("children",3),("goldfish",5),("perfumes",1),("pomeranians",3),("samoyeds",2),("trees",3),("vizslas",0)]}

matches (Sue _ props) (Sue _ toMatch) = all p . HashMap.toList $ props
  where p (prop, count) = case HashMap.lookup prop toMatch of
                            Nothing -> False
                            Just c' -> count == c'

matches2 (Sue _ props) (Sue _ toMatch) = all p . HashMap.toList $ props
  where p (prop, count) = case (HashMap.lookup prop toMatch, prop) of
                            (Nothing, _) -> False
                            (Just c', "cats") -> count > c'
                            (Just c', "trees") -> count > c'
                            (Just c', "pomeranians") -> count < c'
                            (Just c', "goldfish") -> count < c'
                            (Just c', _) -> count == c'

part1 = filter (flip matches machineOutput)
part2 = filter (flip matches2 machineOutput)

main = do
   input <- parseAll <$> readFile "input.txt"
   mapM_ (print . sueIndex) (part1 input)
   mapM_ (print . sueIndex) (part2 input)
