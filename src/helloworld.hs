-- LETS DO IO THE FIRST TIME <3
-- main = putStrLn "hello world"

import Control.Monad
import Data.Char

-- Prelude> :t putStrLn
-- putStrLn :: String -> IO ()
-- this means that putStrLn is a function that takes a string, executes an IO
-- action/side effect and returns an empty tuple (empty tuple is also called
-- unit)
-- TODO(pierre): why is empty tuple called unit?
-- what counts as IO action?
-- - usually reading or writing from/to input/output which will return some
-- value. Since writing to the terminal does usually not yield any meaningful
-- output value, we are using unit/()/empty tuple as return type.
-- the empty tuple is of value () and of type ()

-- when do we perform IO action?
-- - when we define main and run the program

-- with do notation we can glue multiple I/O actions together.

-- main = do
--     putStrLn "hey, who are you?"
--     name <- getLine
--     putStrLn ("hey " ++ name ++ ", you rock!")

-- main always has a type of I/O something (something is concrete type). It will
-- always have to type of the function of the last line in the do notation.
-- Here it is putStrLn which has String -> IO () type.
-- Therefore main was main :: IO ()
-- by convention somehow type of main will not be defined. (ok??)
-- TODO(pierre): is this still the convention? why so?

-- Prelude> :t getLine
-- getLine :: IO String
-- --> getLine is an IO action/side effect which returns a String.
--
-- what is this new syntax? name <- getLine
-- --> means: execute IO action and bind the result to name.
-- Since getLine is IO String, name will be of type String.
-- can think of IO as BOX and in haskell the '<-' is the key that will open the
-- box to find out what was the result of the IO action.
-- NOTE(pierre): the result of an IO action can only be taken out of the box
-- inside a function that is of type IO something. --> this allows for a neat
-- separation of pure and impure stuff!
--
-- why is getLine impure?
-- - because when we execute it twice the result can be different.
--
-- getLine will taint the function, but we can untaint it with the new notation
-- '<-' and then work with the result on untainted/pure functions :D

-- is this valid?
-- - no it is not valid, because ++ only works with string. Here we call ++ on
-- String and IO String.
-- nameTag = "Hello, my name is " ++ getLine

-- is this valid?
-- - YES. Because '<-' can unwrap IO actions/side effects and putStrLn is of
-- type putStrLn :: String -> IO ()
-- --> result will be the value () (and also the type () :])
-- main = do
-- result <- putStrLn "hello world"
-- ...

-- in a do block everything can be bound to. But we skip useless () and only
-- unwrap/bind IO/side effects where we really expect results.
--
-- - to get the value of an IO action we have to use '<-' inside of another IO
-- action.
-- - I/O actions will only be performedd when they are in a function with name
-- "main" OR when we are in a bigger IO action that is composed of do blocks.
-- --> only executed if results eventually fall into main.
-- - AND inside ghci we can do IO :]

-- inside do blocks we can use let bindings without using in.
-- main = do
--     putStrLn "What's your first name?"
--     firstName <- getLine
--     putStrLn "What's your last name?"
--     lastName <- getLine
--     let bigFirstName = map toUpper firstName
--         bigLastName = map toUpper lastName
--     putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- when to use '<-' and when to use let?
-- --> FOR NOW use <- only when unwrapping IO. else use let.

-- read line until input is empty line
-- repeatedRead = do
--     putStrLn "Write some stuff!"
--     result <- getLine
--     if result == "" then putStrLn "finished" else repeatedRead
--
-- main = do
--     repeatedRead

-- forgot the words reverse, solution:
-- main = do
--     line <- getLine
--     if null line
--         then return ()
--         else do
--             putStrLn $ reverseLettersForEachWord $ line
--             main

reverseLettersForEachWord' = unwords $ map reverse $ words "hello world"

-- point free style
reverseLettersForEachWord = unwords . map reverse . words

-- NOTE(pierre): can also call runhaskell filename.hs to compile and execute
-- code.
--
-- NOTE(pierre): if we want to glue togehter IO actions we use do blocks to do
-- so. do notation was required in the else case of our main because the else
-- expects a result of IO action. and we are executing two lines there! But
-- thanks to do notation only the result of the last is used as result

-- NOTE(pierre): IN HASKELL return function is NOTHING like in imperative
-- languages. (c, java, python, etc..). For IO it takes a pure value and returns
-- an IO value.
-- proof that return does behave differently... :]
-- main = do
--     return ()
--     return "HAHAHA"
--     line <- getLine
--     return "BLAH BLAH BLAH"
--     return 4
--     putStrLn line

-- we can use return in combination with <- to bind stuff to names.
-- main = do
--     name1 <- return "hello"

-- OKAY. lets get realer.
-- <- takes a box and unwraps the value from inside the box
-- return takes a value and wraps the value inside a box (based on given type
-- context)

-- in IO do blocks, mostly use return to create IO action to return from main.
-- also used to return a certain value after we finished with our "last" IO
-- action iside our do block. e.g. if we get a good response, we finish with
-- return 0 to get a return value of 0. (here we actually get a return of IO
-- Int, which has the value of 0).
-- IO ACTIONS/FUNCTIONS WE LEARN NOW:
-- writing:
-- putStr -> IO action/side effect that prints given String. No \n /newline at
-- the end.
-- putStrLn -> IO action/side effect that prints given String with \n /newline
-- at the end.
-- putChar -> IO action that prints a given character
-- reading:
-- getStrLn --> IO action/side effect that reads given line (terminated by \n)
-- getChar -> IO action/side effect that reads given character (input/char will
-- only be read once we hit enter!)
-- main = do
--     putStrLn "Insert the correct name to terminate this program.."
--     name <- getLine
--     putStr "hello, "
--     putStrLn name
--     if name == "pierre" then do
--                                 putStrLn "goodbye, pierre!"
--                                 return ()
--                         else do
--                                 putStrLn "wrong name! try again!"
--                                 main

-- print -> takes a value that is instance of Show (meaning that it can be
-- converted to a String), calls show on that value to convert it to string and
-- then basicly calls putStrLn. --> print = putStrLn . show
-- GHCI --> print will be used every time we just type rnd values into terminal
-- and hit enter
-- [1,2,3] equal to print [1,2,3]
-- 1 equal to print 1
-- ghci> map (++"!") ["hey","ho","woo"]
-- ["hey!","ho!","woo!"]
-- ghci> print $ map (++"!") ["hey","ho","woo"]
-- ["hey!","ho!","woo!"]

-- Control.Monad
-- WHEN -> basicly just a function that is a shortcut for
-- if cond then expression else return ()
-- when cond $ expression
-- e.g.
-- main = do
--     c <- getChar
--     when (c /= ' ') $ do
--         putChar c
--         main

-- SEQUENCE -> takes a list of IO actions, performs them and returns a list with
-- results.
-- for lists:
-- sequence :: [IO a] -> IO [a]
-- in general --> sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
-- for my current understanding: Monad --> container that allows certain
-- functions (just a typeclass)
-- Traversable --> typeclass that makes sure that we can traverse over the given
-- data.
-- main = do
--   first <- getLine
--   second <- getLine
--   third <- getLine
--   -- is equal to
--   -- TODO(pierre): is there a way to replicate getLine three times instead of
--   -- writing it like this?
--   results <- sequence [getLine, getLine, getLine]
--   putStrLn "result from three getlines"
--   putStrLn $ first ++ second ++ third
--   putStrLn "result from sequence with list of three getlines"
--   putStrLn $ filter (/= ' ') $ unwords results

-- When do we use sequence?
-- - Sequence is commonly used when we map an IO action over lists/collections.
-- We get a list of IO actions which can then be executed by sequence
-- e.g.
-- sequence $ map (print) [1,2,3,4]
-- Prelude> sequence $ map (print) [1,2,3,4]
-- 1
-- 2
-- 3
-- 4
-- [(),(),(),()]
-- Because this pattern is used commonly there exist functions for this.
-- mapM and mapM_.
-- mapM --> takes a function and a list. maps the function over the list and
-- then sequences it.
-- Prelude> :t mapM
-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- traversable just for lists
-- TODO(pierre): how could we code mapM ? we want to apply f to every value in
-- our list, but don't actually get the result, rather get [f x, f x1,...]?
-- mapM' :: (a -> b) -> [a] -> [b]
-- mapM' f = sequence map f
-- example above with values is eq to
-- mapM print [1,2,3,4]
-- mapM_ does the same, but throws away the result --> [(),(),(),()] :]
-- mapM_ print [1,2,3,4]
-- Prelude> mapM_ print [1,2,3,4]
-- 1
-- 2
-- 3
-- 4


-- forever --> takes an IO action and repeats the IO action it gets forever.
-- main = forever $ do
--     putStrLn "Tell me something!"
--     result <- getLine
--     putStrLn $ map toUpper result


-- forM --> same as mapM, but takes arguments in flipped order.
-- Why/when would we use forM?
-- well.. try to understand this code  now..
-- "
-- sequence unwraps io from the given IO actions and returns the results in a
-- list.
-- TODO(pierre): how would we throw away the result??
-- mapM' f = sequence . map f
-- forM' xs = sequence . (flip map) xs

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
