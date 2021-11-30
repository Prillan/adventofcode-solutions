import           Data.List (nub, subsequences)

isFactor a n = n `mod` a == 0

factor n = factor' 2 n
allFactors = nub . map product . subsequences . factor

factor' a n
  | fromInteger a > sqrt (fromInteger n) = [n]
  | isFactor a n = a:factor' a (n `div` a)
  | otherwise = factor' (a+1) n

score n = 10 * (sum $ allFactors n)
score' n = 11 * (sum $ filter ((<= 50) . (n `div` )) . allFactors $ n)

part1 input = head $ filter ((>= input) . score) [2..]
part2 input = head $ filter ((>= input) . score') [2..]

tests = [ (1, 10)
        , (2, 30)
        , (3, 40)
        , (4, 70)
        , (5, 60)
        , (6, 120)
        , (7, 80)
        , (8, 150)
        , (9, 130) ]

main = do
  let input = 33100000
  -- mapM_ (\(n, s) -> putStrLn $ show (score n == s) ++ " " ++ show n ++ " " ++ show s) tests
  print (part1 input)
  print (part2 input)
