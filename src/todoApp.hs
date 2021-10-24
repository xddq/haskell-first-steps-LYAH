import qualified Data.Char as Char
import qualified System.Environment as Environment
import qualified System.IO as IO

-- TODO APP (view, delete, add) todos for a given file!
-- reason: testing command line arguments.
-- view todos -> ./todo view fileName
-- add todo -> ./todo add fileName textOfTodo
-- delete todo -> ./todo delete fileName numberOfTodo
-- NO ERROR/EXCEPTION CHECKS, this is just to test command line stuff.
-- e.g. view --> runhaskell todo.hs view todo.txt
-- e.g. add --> runhaskell todo.hs add todo.txt "add more todos"
-- e.g. delete --> runhaskell todo.hs delete todo.txt "add more todos"
main = do
  args <- Environment.getArgs
  -- why does hlint want me to use not fail safe function "head" instead of !! ?
  let mode = args !! 0
  let filePath = args !! 1
  case mode of
    "view" -> viewTodos filePath
    "add" -> do
      let newTodo = args !! 2
      appendFile filePath newTodo
    "delete" -> do
      let todo = args !! 2
      deleteTodo filePath todo
    _ ->
      putStrLn "Invalid mode was given. Available modes are [view|add|delete]."

-- helper function that deletes todos by overwriting current file with filtered
-- todos.
deleteTodo filePath todo = do
  todos <- IO.readFile filePath
  -- let newTodos = filter (\line -> line /= todo) $ lines todos
  let newTodos = filter (/= todo) $ lines todos
  if length todos == length newTodos
    then
      putStrLn $
        "Todo "
          ++ todo
          ++ " was not found in your list! Not deleting anything."
    else writeFile filePath $ unlines newTodos

-- helper function to remove elem for given position
removeElemAtPos :: Int -> [a] -> [a]
removeElemAtPos _ [] = []
removeElemAtPos pos xs = leftSide ++ rightSide
  where
    leftSide = take (pos - 1) xs
    rightSide = drop pos xs

-- helper function to read in a given file, number the lines and print them to
-- stdout.
viewTodos filePath = do
  putStrLn "Welcome! Here are your current todos: "
  todos <- IO.readFile filePath
  putStrLn $ numberText todos

-- helper function that takes text and puts numbers in front of every line.
numberText =
  unlines
    . zipWith (\number line -> show number ++ " - " ++ line) [1 ..]
    . lines
