import Data.List (elemIndex)
import System.Environment qualified as SE

main = do
  args <- SE.getArgs
  contents <- readFile $ head args
  let (rules, entries) = parseInput contents
  let ruleCheckers = map checkRule rules
  print $ partOne ruleCheckers entries
  print $ partTwo ruleCheckers entries

partTwo :: [[Integer] -> Bool] -> [[Integer]] -> Integer
partTwo checkers entries =
  let inValidator x = not $ all ($ x) checkers
      inValidEntries = filter inValidator entries
      validEntries = map (fixEntry inValidator) inValidEntries
   in sum $ map middleElement validEntries

fixEntry inValidator = foldl (makeValidInsert inValidator) []

makeValidInsert :: ([Integer] -> Bool) -> [Integer] -> Integer -> [Integer]
makeValidInsert _ [] x = [x]
makeValidInsert isInvalid xs x =
  let insertion = insertAt xs x
   in head $ dropWhile isInvalid (map insertion [0 .. length xs])

insertAt :: [Integer] -> Integer -> Int -> [Integer]
insertAt [] elem pos = [elem]
insertAt (x : xs) elem pos
  | pos == 0 = elem : x : xs
  | pos > 0 = x : insertAt xs elem (pos - 1)

partOne :: [[Integer] -> Bool] -> [[Integer]] -> Integer
partOne checkers entries =
  let validEntries = filter (\x -> all ($ x) checkers) entries
   in sum $ map middleElement validEntries

parseInput :: String -> ([String], [[Integer]])
parseInput content =
  let inputLines = lines content
      rules = takeWhile (/= "") inputLines
      entries = map (map read . splitString ',') (drop (length rules + 1) inputLines)
   in (rules, entries)

checkRule :: String -> [Integer] -> Bool
checkRule s x =
  let (lower, upper) = splitOnce '|' s
      lowerInt = read lower
      upperInt = read upper
      elemMissing = notElem lowerInt x || notElem upperInt x
   in elemMissing || (elemIndex lowerInt x < elemIndex upperInt x)

middleElement x = x !! (length x `div` 2)

splitOnce :: Char -> String -> (String, String)
splitOnce c s =
  let first = takeWhile (/= c) s
      second = drop (length first + 1) s
   in (first, second)

splitString :: Char -> String -> [String]
splitString _ "" = []
splitString c s =
  let (first, second) = splitOnce c s
   in first : splitString c second
