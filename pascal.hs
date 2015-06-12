import Control.Applicative
import Data.List

main = do
  putStrLn "How many rows of pascal's triangle do you want?"
  count <- (\x -> read x :: Int) <$> getLine
  let tree = take count pascalTree
  putStrLn $ treeToString tree


treeToString :: [[Int]] -> String
treeToString tree = concatRows $ map (concat.(map ((++ " ").show))) tree

concatRows :: [String] -> String
concatRows [] = ""
concatRows [x] = x
concatRows (x:xs) =
  let offset = div ((length (last xs)) - (length x)) 2
  in  (replicate offset ' ') ++ x ++ "\n" ++ concatRows xs

pascalTree :: [[Int]]
pascalTree = [1]:[1, 2, 1]:(pascalRows [1, 2, 1])

pascalRows :: [Int] -> [[Int]]
pascalRows row =
  let newRow = pascalRow row
  in  newRow:(pascalRows newRow)

pascalRow :: [Int] -> [Int]
pascalRow previousRow = [1] ++ (innerRow previousRow) ++ [1]

innerRow :: [Int] -> [Int]
innerRow [] = []
innerRow [x] = []
innerRow (x:y:xs) = (x + y):innerRow (y:xs)
