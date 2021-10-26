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
deleteTodo :: String -> String -> IO ()
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
viewTodos :: String -> IO ()
viewTodos filePath = do
  putStrLn "Welcome! Here are your current todos: "
  todos <- IO.readFile filePath
  putStrLn $ numberText todos

-- helper function that takes text and puts numbers in front of every line.
numberText =
  unlines
    . zipWith (\number line -> show number ++ " - " ++ line) [1 ..]
    . lines

-- CONTINUE AT randomness.hs

-- solution:
-- NOTE: dispatch function looks interesting. Just action is also good to catch
-- case when not found.
-- import System.Environment
-- import System.Directory
-- import System.IO
-- import Data.List
--
-- dispatch :: [(String, [String] -> IO ())]
-- dispatch =  [ ("add", add)
--             , ("view", view)
--             , ("remove", remove)
--             ]
--
-- main = do
--     (command:args) <- getArgs
--     let (Just action) = lookup command dispatch
--     action args
--
-- add :: [String] -> IO ()
-- add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
--
-- view :: [String] -> IO ()
-- view [fileName] = do
--     contents <- readFile fileName
--     let todoTasks = lines contents
--         numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
--     putStr $ unlines numberedTasks
--
-- remove :: [String] -> IO ()
-- remove [fileName, numberString] = do
--     handle <- openFile fileName ReadMode
--     (tempName, tempHandle) <- openTempFile "." "temp"
--     contents <- hGetContents handle
--     let number = read numberString
--         todoTasks = lines contents
--         newTodoItems = delete (todoTasks !! number) todoTasks
--     hPutStr tempHandle $ unlines newTodoItems
--     hClose handle
--     hClose tempHandle
--     removeFile fileName
--     renameFile tempName fileName
--
--
--
-- CONTINUE AT randomness.hs
