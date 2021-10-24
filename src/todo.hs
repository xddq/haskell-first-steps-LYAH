-- continue with book learnyouahaskell
-- src: http://learnyouahaskell.com/input-and-output
-- todo.txt

import qualified Data.Char as Char
import qualified System.Environment as Environment
import qualified System.IO as IO

-- take line from stdin and add it to our file.
-- main = do
--   line <- getLine
--   appendFile "./todo.txt" $ line ++ "\n"

-- BUFFERING
-- - by default it is lazy I/O actions with line-based buffering
-- e.g: Prints out the given text line by line (original line, read line,
-- original line +1, read line +1, etc...)
-- - we can think of it as a pipe that is connected on one end with the file and
-- on the other end with our handle.
-- - the buffering could be described by the diameter of the pipe (given
-- constant input/check speed and constant read speed) when we think of a
-- literal pipe.
-- - for binary files it is block sized buffering (default set by the os, maybe
-- the fs?)
-- main = do
--     withFile "something.txt" ReadMode (\handle -> do
--         contents <- hGetContents handle
--         putStr contents)
--
-- Buffering can be controlled by setting hSetBuffering for a given BufferMode and Handle/pipe.
-- NoBuffering, LineBuffering or BlockBuffering (Maybe Int). (Maybe Int -> if it
-- is Nothing it will be set by the OS)

-- same code with block sized buffering. reads 2048 bytes per block.
-- main = do
--     IO.withFile "./testfile" IO.ReadMode (\handle -> do
--         IO.hSetBuffering handle $ IO.BlockBuffering $ Just 2048
--         contents <- IO.hGetContents handle
--         putStr $ contents)
-- Why would we use block buffering instead of line buffering?
-- - Disk IO or Network IO is costly, if this is relevant then prefer reading
-- bigger chunks.

-- hFlush
-- can be used to flush the buffer (clear buffer and have a result with
-- everything that is currently inside it)

-- code program which will print a list of lines and allow us to delete a
-- specific line. (used for deleting todos inside a file where each line is a
-- todo)
-- main = do
--   putStrLn "Welcome! Here are your current todos: "
--   todos <- IO.readFile "./todo.txt"
--   putStrLn $ numberText todos
--   putStrLn "Which one do you want to delete? Give the number and hit enter"
--   lineNumber <- getChar
--   let filteredTodos = unlines $ removeElemAtPos (Char.digitToInt lineNumber) (lines todos)
--   putStrLn "Done. The todos that are left are:"
--   putStrLn $ numberText filteredTodos
--   -- TODO(pierre): does IO.writeFile close the handle created with
--   -- IO.readFile ???
--   IO.writeFile "./todo.txt" filteredTodos
--   return ()

-- helper function that takes text and puts numbers in front of every line.
-- numberText = unlines . zipWith (\number line -> show number ++ " - " ++ line) [1 ..] . lines
--
-- -- helper function to remove elem for given position
-- removeElemAtPos :: Int -> [a] -> [a]
-- removeElemAtPos _ [] = []
-- removeElemAtPos pos xs = leftSide ++ rightSide
--   where
--     leftSide = take (pos -1) xs
--     rightSide = drop pos xs

-- solution:
-- NOTES:
-- - I implemented the delete function with removeElemAtPos xd..
-- - why does he create separate files for read and write? Here we read and then
-- write. Could be done with one file?
-- - openTempFile --> used to open a temporary file to avoid mistakes when
-- reading/writing to the same file!
-- how can we specify the current directory for the temp directory?
-- - by giving the path "."
-- "
-- import System.IO
-- import System.Directory
-- import Data.List
-- main = do
--     handle <- openFile "todo.txt" ReadMode
--     (tempName, tempHandle) <- openTempFile "." "temp"
--     contents <- hGetContents handle
--     let todoTasks = lines contents
--         numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
--     putStrLn "These are your TO-DO items:"
--     putStr $ unlines numberedTasks
--     putStrLn "Which one do you want to delete?"
--     numberString <- getLine
--     let number = read numberString
--         newTodoItems = delete (todoTasks !! number) todoTasks
--     hPutStr tempHandle $ unlines newTodoItems
--     hClose handle
--     hClose tempHandle
--     removeFile "todo.txt"
--     renameFile tempName "todo.txt"
--     "
--     NOTE: removeFile and renameFile take file paths and not handles. (Because
--     with handles we would delete the handle/pipe and not the file :])

-- COMMAND LINE ARGUMENTS
-- from System.Environment
-- main = do
--    args <- Environment.getArgs
--    progName <- Environment.getProgName
--    putStrLn "The arguments are:"
--    -- TODO(pierre): why do we not see the [(),(),..n] result from mapM here?
--    mapM putStrLn args
--    putStrLn "The program name is:"
--    putStrLn progName

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
    otherwise -> putStrLn "Invalid mode was given. Available modes are [view|add|delete]."

-- helper function that deletes todos by overwriting current file with filtered
-- todos.
deleteTodo filePath todo = do
  todos <- IO.readFile filePath
  let newTodos = filter (\line -> line /= todo) $ lines todos
  case length todos == length newTodos of
    True -> putStrLn $ "Todo " ++ todo ++ " was not found in your list! Not deleting anything."
    False -> writeFile filePath $ unlines newTodos

-- helper function to remove elem for given position
removeElemAtPos :: Int -> [a] -> [a]
removeElemAtPos _ [] = []
removeElemAtPos pos xs = leftSide ++ rightSide
  where
    leftSide = take (pos -1) xs
    rightSide = drop pos xs

-- helper function to read in a given file, number the lines and print them to
-- stdout.
viewTodos filePath = do
  putStrLn "Welcome! Here are your current todos: "
  todos <- IO.readFile filePath
  putStrLn $ numberText todos

-- helper function that takes text and puts numbers in front of every line.
numberText = unlines . zipWith (\number line -> show number ++ " - " ++ line) [1 ..] . lines

-- continue with coding own todo app based on description in todoApp.hs
-- CONTINUE in todoApp.hs
