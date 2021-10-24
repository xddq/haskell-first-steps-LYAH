-- continue with book learnyouahaskell
-- src: http://learnyouahaskell.com/input-and-output
-- todo.txt
import qualified System.IO as IO
import qualified Data.Char as Char

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
main = do
    putStrLn "Welcome! Here are your current todos: "
    todos <- IO.readFile "./todo.txt"
    putStrLn $ numberText todos
    putStrLn "Which one do you want to delete? Give the number and hit enter"
    lineNumber <- getChar
    let filteredTodos = unlines $ removeElemAtPos (Char.digitToInt lineNumber) (lines $ todos)
    putStrLn "Done. The todos that are left are:"
    putStrLn $ numberText filteredTodos
    IO.writeFile "./todo.txt" filteredTodos
    return ()

-- helper function that takes text and puts numbers in front of every line.
numberText = unlines . zipWith (\number line -> show number ++ " - " ++ line) [1..] . lines

-- helper function to remove elem for given position
removeElemAtPos :: Int -> [a] -> [a]
removeElemAtPos _ [] = []
removeElemAtPos pos xs = leftSide ++ rightSide
    where leftSide = take (pos-1) xs
          rightSide = drop pos xs
