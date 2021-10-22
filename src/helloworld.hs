-- LETS DO IO THE FIRST TIME <3
-- main = putStrLn "hello world"
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
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseLettersForEachWord $ line
            main

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
-- STOP HERE. continue at PutStr.
