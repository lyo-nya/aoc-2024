import Data.List (sort)
import System.Environment qualified as SE

main = do
  args <- SE.getArgs
  contents <- readFile $ head args
  let records = parseRecords contents
  print $ partOne records
  print $ partTwo records

partOne :: [[Integer]] -> Int
partOne records = length $ filter isSafe records

partTwo :: [[Integer]] -> Int
partTwo records = length $ filter (\v -> isSafe v || canBeSafe v) records

parseInts :: [String] -> [Integer]
parseInts = map read

parseRecords :: String -> [[Integer]]
parseRecords content = map (parseInts . words) (lines content)

isSafe :: [Integer] -> Bool
isSafe x =
  let diffs = zipWith subtract x (tail x)
   in all (between (-4) 0) diffs || all (between 0 4) diffs

canBeSafe :: [Integer] -> Bool
canBeSafe x = any (isSafe . removeElement x) [0 .. length x]

removeElement :: [a] -> Int -> [a]
removeElement [] _ = []
removeElement (x : xs) 0 = xs
removeElement (x : xs) n = x : removeElement xs (n - 1)

between :: Integer -> Integer -> Integer -> Bool
between a b x = x > a && x < b
