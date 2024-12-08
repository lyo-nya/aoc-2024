import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.List (groupBy, nub, sortOn)
import System.Environment qualified as SE

main = do
  args <- SE.getArgs
  contents <- readFile $ head args
  print $ partOne contents
  print $ partTwo contents

partOne :: String -> Int
partOne x =
  let matrix = lines x
      width = length (head matrix) - 1
      height = length matrix - 1
      combinations = concatMap getCombinations (groupBy ((==) `on` thrd) (getPositions matrix))
      candidates = concatMap (getAntinodesPosition 1 False) combinations
      height_filtered = filter (\(x, y) -> between 0 height y) candidates
      width_filtered = filter (\(x, y) -> between 0 width x) height_filtered
   in length $ nub width_filtered

partTwo :: String -> Int
partTwo x =
  let matrix = lines x
      width = length (head matrix) - 1
      height = length matrix - 1
      combinations = concatMap getCombinations (groupBy ((==) `on` thrd) (getPositions matrix))
      candidates = concatMap (getAntinodesPosition 100 True) combinations
      height_filtered = filter (\(x, y) -> between 0 height y) candidates
      width_filtered = filter (\(x, y) -> between 0 width x) height_filtered
   in length $ nub width_filtered

between a b x = x >= a && x <= b

getPositions :: [String] -> [(Int, Int, Char)]
getPositions x = sortOn thrd (filterTuplesLastValue isAlphaNum (getIndexedList x))

filterTuplesLastValue p = filter (\(_, _, v) -> p v)

getIndexedList :: [String] -> [(Int, Int, Char)]
getIndexedList x = concatMap (\(i, s) -> zipWith (formTuple i) [0 ..] s) (zip [0 ..] x)

dropLastFromTuple (a, b, c) = (a, b)

thrd (a, b, c) = c

formTuple a b c = (a, b, c)

getCombinationsWith :: (Int, Int, Char) -> [(Int, Int, Char)] -> [(Int, Int, Int, Int)]
getCombinationsWith x [] = []
getCombinationsWith (x1, y1, _) [(x2, y2, _)] = [(x1, y1, x2, y2)]
getCombinationsWith x xs = getCombinationsWith x [head xs] ++ getCombinationsWith x (tail xs)

getCombinations :: [(Int, Int, Char)] -> [(Int, Int, Int, Int)]
getCombinations [] = []
getCombinations (x : xs) = getCombinationsWith x xs ++ getCombinations xs

getAntinodesPosition :: Int -> Bool -> (Int, Int, Int, Int) -> [(Int, Int)]
getAntinodesPosition n take_self (x1, y1, x2, y2) =
  let top = min y1 y2
      bottom = max y1 y2
      left = min x1 x2
      right = max x1 x2
      x_delta = (right - left)
      y_delta = (bottom - top)
      main_diag = x1 < x2 && y1 < y2
      first =
        if main_diag
          then map (\i -> (left - i * x_delta, top - i * y_delta)) [1, 2 ..]
          else map (\i -> (left - i * x_delta, bottom + i * y_delta)) [1, 2 ..]
      second =
        if main_diag
          then map (\i -> (right + i * x_delta, bottom + i * y_delta)) [1, 2 ..]
          else map (\i -> (right + i * x_delta, top - i * y_delta)) [1, 2 ..]
   in take n first ++ take n second ++ if take_self then [(x1, y1), (x2, y2)] else []

-- 301 – 314
