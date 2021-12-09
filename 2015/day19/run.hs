{-# LANGUAGE BangPatterns #-}
import           Data.Aeson
import           Data.List (permutations, group, minimum, maximum, minimumBy, maximumBy, isPrefixOf, sortBy)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Debug.Trace (trace)
import           Text.Parsec

--newtype Atom = Atom String deriving (Show, Eq)
newtype Molecule = Molecule String deriving (Show, Eq)

data Rule = Rule !Molecule !Molecule deriving Show

azl = ['a'..'z']
azu = ['A'..'Z']

--atom = Atom <$> ((:) <$> oneOf ('e':azu) <*> many (oneOf azl))
molecule = Molecule <$> many1 (oneOf $ azl ++ azu)
rule = Rule <$> molecule <*> (string " => " *> molecule)

unsafeRight (Right x) = x

parseAll input = ( map unsafeRight . map (parse rule "") $ r
                 , filter (`elem` (azl++azu)) $ l)
  where l = last . lines $ input
        r = init . filter ((>= 1) . length) . lines $ input


replace :: String -> String -> String -> [String]
replace f t v = replace' [] v
  where replace' _ [] = []
        replace' l r@(rh:rs)
          | f `isPrefixOf` r = ((reverse l) ++ t ++ (drop (length f) r)) : replace' (rh:l) rs
          | otherwise = replace' (rh:l) rs

replaceFirst :: String -> String -> String -> Maybe String
replaceFirst f t v = replace' [] v
  where replace' _ [] = Nothing
        replace' l r@(rh:rs)
          | f `isPrefixOf` r = Just ((reverse l) ++ t ++ (drop (length f) r))
          | otherwise = replace' (rh:l) rs

applyMany input (Rule (Molecule from) (Molecule to)) = replace from to input
step :: [Rule] -> String -> Set.Set String
step rules input = Set.fromList $ concatMap (applyMany input) rules

reverseRule (Rule m1 m2) = Rule m2 m1
noE (Rule (Molecule m1) _) = m1 /= "e"
bigTo (Rule _ (Molecule m1)) (Rule _ (Molecule m2)) = length m2 `compare` length m1

part1 rules input = step rules input
part2 rules input = filter (\(i, s) -> trace (show i) $ Set.member input s) values
  where step' :: Set.Set String -> Set.Set String
        step' = Set.unions . map (Set.filter ((<= l) . length) . step rules) . Set.toList
        values = zip [0..] $ iterate step' (Set.fromList ["e"])
        l = length input

part2' rules input =
  filter (\(i, (s)) -> trace (show i) $ not . Set.null $ Set.intersection s targets) values
  where targets = Set.fromList ["HF", "Nal", "OMg"]
        rules' = map reverseRule . filter noE . sortBy bigTo $ rules
        step' :: (Set.Set String) -> (Set.Set String)
        step' (toCheck) = (new)
          where new = (
                 Set.unions
                 . map (step rules')
                 . Set.toList $ toCheck)
        values = zip [0..] $ iterate step' (Set.fromList [input])


part2'' rules input = takeWhile (/= "e") $ iterate reduce input
  where reduce v = head $ mapMaybe (\(Rule (Molecule f) (Molecule t)) -> replaceFirst f t v) reversed
        reversed = map reverseRule $ rules
        

main = do
   (rules, input) <- parseAll <$> readFile "input.txt"
   -- mapM_ print rules
   -- print input
   print (length $ part1 rules input)
   mapM_ print (part2'' rules input)
