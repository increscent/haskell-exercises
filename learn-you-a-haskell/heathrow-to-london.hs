import System.Environment

type StreetCorners = (Int, Int)
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

main = do
  args <- getArgs
  contents <- readFile $ head args
  putStrLn $ "The shortest path is " ++ (show $ parseInput contents)

parseInput :: String -> Int
parseInput input =
  let allLines = lines input
      distances = map (\value -> read value :: Int) allLines
  in  shortestPath distances (0, 0)

shortestPath :: [Int] -> StreetCorners -> Int
shortestPath (aDist:bDist:between:rest) (currentA, currentB)
  | between == 0 = (min aDist bDist) + (min currentA currentB)
  | otherwise = shortestPath rest (shortestA, shortestB)
  where shortestCurrent = min currentA currentB
        shortestDist = min aDist bDist
        shortestA = min (currentA + aDist) (currentB + bDist + between)
        shortestB = min (currentB + bDist) (shortestA + between)
