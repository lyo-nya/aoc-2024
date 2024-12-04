import Data.List (transpose)
import System.Environment qualified as SE
import Text.ParserCombinators.ReadP (count)

main = do
  args <- SE.getArgs
  contents <- readFile $ head args
  print $ partOne contents
  print $ partTwo contents

partOne = sum . map countXMASes . transformLines . lines

partTwo = countXes . lines

countXes :: [String] -> Int
countXes x
  | length x < 3 = 0
  | length (head x) < 3 = 0
  | length x == 3 =
      let sub = map (take 3) x
          tr = matrixTrace sub
          backTr = matrixTrace (map reverse sub)
          backSlashMatches = elem "MAS" [tr, reverse tr]
          slashMatches = elem "MAS" [backTr, reverse backTr]
          xFound = backSlashMatches && slashMatches
          count = if xFound then 1 else 0
       in count + countXes (map tail x)
  | otherwise = countXes (take 3 x) + countXes (tail x)

transformLines :: [String] -> [String]
transformLines s =
  let t = transpose s
      t_reversed = map reverse t
      s_reversed = map reverse s
      ut = upperTriangle s
      lt = lowerTriangle $ drop 1 s
      ut_reversed = upperTriangle s_reversed
      lt_reversed = lowerTriangle $ drop 1 s_reversed
      res = s ++ t ++ ut ++ lt ++ ut_reversed ++ lt_reversed
   in res ++ map reverse res

matrixTrace :: [String] -> String
matrixTrace [] = []
matrixTrace (x : xs) = head x : matrixTrace (map tail xs)

lowerTriangle :: [String] -> [String]
lowerTriangle [] = []
lowerTriangle x = matrixTrace x : lowerTriangle (drop 1 x)

upperTriangle = lowerTriangle . transpose

countXMASes :: String -> Int
countXMASes content
  | length content < 4 = 0
  | take 4 content == "XMAS" = 1 + countXMASes (drop 4 content)
  | otherwise = countXMASes $ tail content
