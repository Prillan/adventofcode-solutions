{-# LANGUAGE LambdaCase #-}

import Data.List
import Data.List.Split
import Data.Maybe (mapMaybe)

data FileSystem = FsDir String [FileSystem]
                | FsFile String Int
  deriving Show

type FlatFs = [([String], String, Int)]

data Command = Cd String
             | Ls [Entry]
  deriving Show
data Entry = Dir String
           | File String Int
  deriving Show

parse :: String -> Maybe Command
parse input =
  case lines input of
    "ls":output -> pure . Ls $ map (f . words) output
      where f = \case ["dir", name] -> Dir name
                      [size, name] -> File name (read size)
    ['c':'d':' ':target] -> pure $ Cd target
    _ -> Nothing

parseAll :: String -> [Command]
parseAll =
  mapMaybe parse
  . filter (not . null)
  . splitOn "\n$ "

exec :: [Command] -> FlatFs
exec = go [] []
  where go cwd acc =
          \case Ls es:rest -> go cwd (acc ++ concatMap (f cwd) es) rest
                Cd s:rest -> go (nav cwd s) acc rest
                [] -> acc
                
        f cwd = \case File fname size -> [(reverse cwd, fname, size)]
                      _ -> []

nav :: [String] -> String -> [String]
nav (_:xs) ".." = xs
nav xs x = x:xs

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

buildTree :: FlatFs -> FileSystem
buildTree = go "/" . reverse . sort
  where go name entries =
          let groups = groupBy (\x y -> take 1 (fst3 x) == take 1 (fst3 y)) entries
          in
            FsDir name (concatMap dirEntries groups)
        dirEntries =
          \case entries@((name:_, _ ,_ ):_) -> [go name (map dropDir entries)]
                files -> map (\(_, fname, size) -> FsFile fname size) files


        dropDir =
          \case e@([],_,_) -> e
                (_:xs, f, s) -> (xs, f, s)

size :: FileSystem -> Int
size = \case FsFile _ s -> s
             FsDir _ es -> sum (map size es)

collect :: (Int -> Bool) -> FileSystem -> [(String, Int)]
collect p = go
  where go =
          \case FsDir name entries ->
                  let s = sum (map size entries)
                  in
                    if p s
                    then (name, s):concatMap go entries
                    else concatMap go entries
                _ -> []

part1 :: FileSystem -> Int
part1 = sum . map snd . collect (< 10000) 

part2 :: FileSystem -> Int
part2 fs =
  let used = size fs
      total = 70000000
      unused = total - used
      needToDelete = 30000000 - unused
  in
    head
    . sort
    . map snd
    $ collect (>= needToDelete) fs

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   let tree = buildTree (exec input)
   print (part1 tree)
   print (part2 tree)
