import System.IO
import System.Directory
import Data.List

main = do
  putStrLn "\nHere are your todos:"
  contents <- readFile "todo.txt"
  putStrLn contents
  putStrLn "What do you want to do (add, delete, quit)?"
  choice <- getLine
  if (choice == "add")
    then do
      addTodo
      main
    else if (choice == "delete")
      then do
        deleteTodo
        main
      else return ()

addTodo :: IO ()
addTodo = do
  putStrLn "What task do you want to add?"
  task <- getLine
  handle <- openFile "todo.txt" ReadMode
  (tmpName, tmpHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  hPutStrLn tmpHandle task
  hPutStr tmpHandle contents
  hClose tmpHandle
  hClose handle
  removeFile "todo.txt"
  renameFile tmpName "todo.txt"

deleteTodo :: IO ()
deleteTodo = do
  handle <- openFile "todo.txt" ReadMode 
  contents <- hGetContents handle
  let tasks = lines contents
      enumerated_tasks = zipWith (\n line -> show n ++ " -- " ++ line) [0..] tasks
  mapM putStrLn enumerated_tasks
  putStrLn "Which task do you want to delete?"
  numberString <- getLine
  let new_tasks = deleteByIndex (read numberString :: Int) tasks
  hClose handle
  let tasks_list = foldl (\acc x -> if (acc == []) then x else acc ++ '\n':x) "" new_tasks
  writeFile "todo.txt" tasks_list

deleteByIndex :: Int -> [a] -> [a]
deleteByIndex a [] = []
deleteByIndex a (x:xs)
  | a == 0 = xs
  | otherwise = x:(deleteByIndex (a - 1) xs)
