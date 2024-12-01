import Data.List (sort)
import System.Environment qualified as SE

main = do
  args <- SE.getArgs
  contents <- readFile $ head args
  let records = parseRecords contents
  print $ partOne records
  print $ partTwo records

parseInts :: [String] -> [Integer]
parseInts = map read

parseRecords :: String -> [[Integer]]
parseRecords content = map (parseInts . words) (lines content)

absDiff :: Integer -> Integer -> Integer
absDiff a b = max a b - min a b

partOne :: [[Integer]] -> Integer
partOne records =
  let diffs = zipWith absDiff first second
      first = sort $ map (!! 0) records
      second = sort $ map (!! 1) records
   in sum diffs

countOccurances :: [Integer] -> [(Integer, Integer)]
countOccurances [] = []
countOccurances [x] = [(x, 1)]
countOccurances (x : xs) =
  let head_count = fromIntegral $ (length . filter (== x)) xs
      head = (x, head_count + 1)
      tail = countOccurances $ filter (/= x) xs
   in head : tail

matchOccurances :: [(Integer, Integer)] -> [Integer] -> Integer
matchOccurances [] _ = 0
matchOccurances [(x, cnt)] v =
  let other_cnt = fromIntegral $ length $ filter (== x) v
   in x * cnt * other_cnt
matchOccurances ((x, cnt) : xs) v =
  let other_cnt = fromIntegral $ length $ filter (== x) v
      tail = matchOccurances xs (filter (/= x) v)
   in x * cnt * other_cnt + tail

partTwo :: [[Integer]] -> Integer
partTwo records =
  let first = map (!! 0) records
      second = map (!! 1) records
   in matchOccurances (countOccurances first) second
