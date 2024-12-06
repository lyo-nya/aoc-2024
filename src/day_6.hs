import Control.Parallel.Strategies
import Data.List (transpose)
import System.Environment qualified as SE

main = do
  args <- SE.getArgs
  contents <- readFile $ head args
  print $ partOne contents
  print $ partTwo contents

partOne = length . getPositions . play . lines

partTwo :: String -> Int
partTwo contents =
  let gameState = lines contents
      positions = getPositions gameState
      startingPosition = head positions
      candidates = (filter (/= startingPosition) . getPositions . play) gameState
      results = parMap rpar (willGetStuck [] . insertBlock gameState) candidates
   in length $ filter id results

insertBlock l (x, y) =
  let newLine = replaceAt (l !! x) '#' y
   in replaceAt l newLine x

willGetStuck :: [(Int, Int, Char)] -> [String] -> Bool
willGetStuck knownStates gameState
  | pos `elem` knownStates = True
  | gameOver gameState = False
  | otherwise = willGetStuck (pos : knownStates) (move gameState pos)
  where
    pos = getGuardPosition gameState

getGuardPosition :: [String] -> (Int, Int, Char)
getGuardPosition x = head $ filterTuplesLastValue (`elem` "<>^v") $ getIndexedList x

getPositions :: [String] -> [(Int, Int)]
getPositions x = map dropLastFromTuple (filterTuplesLastValue (`elem` "X<>^v") $ getIndexedList x)

getIndexedList :: [String] -> [(Int, Int, Char)]
getIndexedList x = concatMap (\(i, s) -> zipWith (formTuple i) [0 ..] s) (zip [0 ..] x)

dropLastFromTuple (a, b, c) = (a, b)

formTuple a b c = (a, b, c)

filterTuplesLastValue p = filter (\(_, _, v) -> p v)

play :: [String] -> [String]
play gameState
  | gameOver gameState = gameState
  | otherwise = play $ move gameState pos
  where
    pos = getGuardPosition gameState

horizontalMove :: Char -> String -> String
horizontalMove _ "" = ""
horizontalMove _ [x] = [x]
horizontalMove '>' (first : second : rest)
  | first == '>' && second == '#' = 'v' : '#' : rest
  | first == '>' && second /= '#' = 'X' : horizontalMove '>' ('>' : rest)
  | otherwise = first : horizontalMove '>' (second : rest)
horizontalMove '<' x =
  let reversed = replaceChar '<' '>' (reverse x)
      moved = horizontalMove '>' reversed
   in reverse $ (replaceChar 'v' '^' . replaceChar '>' '<') moved
horizontalMove c x
  | c `elem` x = verticalMove c x
  | otherwise = x

verticalMove :: Char -> String -> String
verticalMove 'v' x =
  let replaced = replaceChar 'v' '>' x
      moved = horizontalMove '>' replaced
      replacedBack = (replaceChar '>' 'v' . replaceChar 'v' '<') moved
   in replacedBack
verticalMove '^' x =
  let replaced = replaceChar '^' '<' x
      moved = horizontalMove '<' replaced
      replacedBack = (replaceChar '<' '^' . replaceChar '^' '>') moved
   in replacedBack
verticalMove c x
  | c `elem` x = verticalMove c x
  | otherwise = x

move :: [String] -> (Int, Int, Char) -> [String]
move x (i, j, d)
  | d == '>' =
      let r = horizontalMove '>' (x !! i)
       in replaceAt x r i
  | d == '<' =
      let r = horizontalMove '<' (x !! i)
       in replaceAt x r i
  | d == 'v' =
      let r = verticalMove 'v' (t !! j)
       in transpose $ replaceAt t r j
  | d == '^' =
      let r = verticalMove '^' (t !! j)
       in transpose $ replaceAt t r j
  where
    t = transpose x

gameOver :: [String] -> Bool
gameOver scheme
  | '^' `elem` top = True
  | 'v' `elem` bottom = True
  | '<' `elem` left = True
  | '>' `elem` right = True
  | otherwise = False
  where
    top = head scheme
    bottom = last scheme
    left = map head scheme
    right = map last scheme

replaceAt :: [a] -> a -> Int -> [a]
replaceAt [] elem pos = [elem]
replaceAt (x : xs) elem pos
  | pos == 0 = elem : xs
  | pos > 0 = x : replaceAt xs elem (pos - 1)

replaceChar _ _ [] = []
replaceChar c1 c2 x
  | head x == c1 = c2 : tail x
  | otherwise = head x : replaceChar c1 c2 (tail x)
