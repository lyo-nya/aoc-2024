import System.Environment qualified as SE

main = do
  args <- SE.getArgs
  contents <- readFile $ head args
  print $ solve getCandidatesPartOne contents
  print $ solve getCandidatesPartTwo contents

solve f x = sum $ map (validationValue f . parseLine) (lines x)

getCandidatesPartOne (x, y : z : rest) = [(x, (y + z) : rest), (x, (y * z) : rest)]

getCandidatesPartTwo (x, y : z : rest) = [(x, (y + z) : rest), (x, (y * z) : rest), (x, concatIntegers y z : rest)]

parseLine :: String -> (Integer, [Integer])
parseLine x =
  let ws = words x
      target = read $ init $ head ws
      nums = map read (tail ws)
   in (target, nums)

validationValue :: ((Integer, [Integer]) -> [(Integer, [Integer])]) -> (Integer, [Integer]) -> Integer
validationValue _ (x, []) = 0
validationValue _ (x, [y]) = if x == y then x else 0
validationValue f x
  | fst x < head (snd x) = 0
  | otherwise = maximum $ map (validationValue f) (f x)

concatIntegers :: Integer -> Integer -> Integer
concatIntegers x y = read (show x ++ show y)
