-- src: http://learnyouahaskell.com/functionally-solving-problems

import Control.Monad
import Data.Char
import qualified Data.Char as Char
import Data.Maybe

testArray = "10 4 2 * -"

-- RPN - REVERSE POLISH NOTATION
-- calculating by putting onto stack until we find an operator and then apply
-- that operator to the (depending on arity, mostly 2 I guess?) two values which
-- are on the stack and we put the result back on the stack. when we are on the
-- end we pop the stack. e.g.
-- RPN 10 4 3 + 2 * - --> 3+4=7;2*7=14;14-10=4; 4

calcRPN :: String -> IO String
calcRPN input = do
  putStrLn "hello"
  return "hello"

-- main = forever $ do
--   putStrLn "Enter your expression in RPN (reverse polish notation)."
--   putStrLn "Example: 10 4 3 + 2 * - "
--   input <- getLine
--   putStrLn $ "input was: " ++ input
--   let result = show $ calc input EmptyStack
--   putStrLn $ "result is: " ++ result

-- TODO: why does this not work the same as above?
-- putStrLn "Enter your expression in RPN (reverse polish notation)."
-- putStr "Example: 10 4 3 + 2 * - "
-- interact (\input -> show $ calc input EmptyStack)

-- calculates RPN recursively
calc :: (Num a, Read a, Show a, Fractional a) => String -> Stack a -> a
calc [] stack = result
  where
    (result, restStack) = stackPop stack
calc input stack = do
  let values = words input
  let nextValue = head values
  let nextInput = unwords $ drop 1 values
  if representsNumber nextValue
    then calc nextInput (stackPush stack $ read nextValue)
    else case returnOperator nextValue of
      Just operator -> do
        let (firstVal, secondVal, restStack) = popTwoValues stack
        calc nextInput (stackPush restStack (operator firstVal secondVal))
      Nothing -> undefined

representsNumber :: String -> Bool
representsNumber = all ((== True) . Char.isNumber)

popTwoValues :: Stack a -> (a, a, Stack a)
popTwoValues stack = (firstValue, secondValue, restStack')
  where
    (firstValue, restStack) = stackPop stack
    (secondValue, restStack') = stackPop restStack

returnOperator :: (Num a, Fractional a) => String -> Maybe (a -> a -> a)
returnOperator input
  | input == "+" = Just (+)
  | input == "-" = Just (-)
  | input == "*" = Just (*)
  | input == "/" = Just (/)
  | otherwise = Nothing

infixr 5 :-:

data Stack a = EmptyStack | a :-: (Stack a) deriving (Show, Eq, Read, Ord)

makeStack :: a -> Stack a
makeStack input = input :-: EmptyStack

stackPush :: Stack a -> a -> Stack a
stackPush xs input = input :-: xs

testStack = stackPush (stackPush (makeStack 10) 2) 3

stackPop :: Stack a -> (a, Stack a)
stackPop EmptyStack = undefined
stackPop (x :-: xs) = (x, xs)

-- SOLUTION to RPN:
-- call operation on first two values in list --> using list as our stack first
-- entry is the top of stack.
-- when we have a number we just prepend it to list --> add it to stack
-- NOTE: not failsafe yet. will be adapted once we get to know monads.
solveRPN :: (Read a, Num a) => String -> a
-- solveRPN expression = head (foldl foldingFunction [] (words expression))
solveRPN = head . foldl foldingFunction [] . words
  where
    foldingFunction (x : y : ys) "*" = (x * y) : ys
    foldingFunction (x : y : ys) "+" = (x + y) : ys
    foldingFunction (x : y : ys) "-" = (y - x) : ys
    -- expect it to be multiple numbers represented as string if it is not one
    -- of our symbols [*,+,-,..]
    foldingFunction xs numberString = read numberString : xs

-- HEATHROW TO LONDON
-- shortest path from a to b.
-- we only have to roads and for each intersection we have one crossroad.
-- check the picture to understand:
-- http://learnyouahaskell.com/functionally-solving-problems
type Road a b = Either a b

data Edge a b c = Edge String String Int deriving (Show, Read, Eq)

data RoadA a = RoadA Int

data RoadB a = RoadB Int

-- createEdges :: (String, String) -> Int -> Int -> Int -> [Edge]
-- createEdges (startA, startB) costAtoA costAtoB costBtoB = do
--
--
-- MAYBE(pierre): do HEATHROW TO LONDON
-- NOTE(pierre): Skipped the second exercise. Rather focus on getting the next
-- chapter (functors, applicative functors, monoids, etc.. and later return to
-- the exercise. It is just about the problem, not really about haskell specific
-- stuff.
-- CONTINUE AT: FunctorsApplicativeFunctorsMonoids.hs
