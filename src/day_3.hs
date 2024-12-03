import Data.List (sort)
import GHC.Conc (par)
import System.Environment qualified as SE
import Text.Regex.Posix

mulRegEx = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"

numRegEx = "[0-9]{1,3}"

main = do
  args <- SE.getArgs
  contents <- readFile $ head args
  print $ partOne contents
  print $ partTwo contents

partTwo = partOne . cleanOperations

partOne :: String -> Integer
partOne x = sum $ map applyMul $ parseOperations x

parseOperations :: String -> [String]
parseOperations content = getAllTextMatches (content =~ mulRegEx) :: [String]

cleanOperations :: String -> String
cleanOperations [] = []
cleanOperations x = takeUntilDont x

takeUntilDont :: String -> String
takeUntilDont [] = []
takeUntilDont x
  | take 7 x == "don't()" = '@' : skipUntillDo (drop 7 x)
  | otherwise = head x : takeUntilDont (tail x)

skipUntillDo :: String -> String
skipUntillDo [] = []
skipUntillDo x
  | take 4 x == "do()" = '@' : takeUntilDont (drop 4 x)
  | otherwise = skipUntillDo (tail x)

applyMul :: String -> Integer
applyMul x = product (map read (getAllTextMatches (x =~ numRegEx) :: [String]))
