-- src: http://learnyouahaskell.com/functionally-solving-problems
-- problem: RPN (reverse polish notation) --> calculating by putting onto stack
-- until we find an operator and then apply that operator to the (depending on
-- arity, mostly 2 I guess?) two values which are on the stack and we put the
-- result back on the stack. when we are on the end we pop the stack.
-- e.g.
-- RPN 10 4 3 + 2 * - --> 3+4=7;2*7=14;14-10=4; 4
import qualified Data.Char as Char
import Data.Maybe

testArray = "10 4 2 * -"

--
calcRPN :: (Num a) => String -> a
calcRPN input = do
  let numbers = filter representsNumber $ words input
  let operators = filter representsOperator $ words input
  let order = map representsNumber $ words input
  3

data Action = PushStack | PopStack deriving (Show, Eq)

makeStack :: a -> Stack a
makeStack input = input :-: EmptyStack

calc :: (Num a, Read a, Fractional a) => String -> Stack a -> a
calc [] stack = result
  where
    (result, restStack) = stackPop stack
calc input stack = do
  let nextValue = head $ words input
  if representsNumber nextValue
    then calc (drop 1 input) (stackPush stack $ read nextValue)
    else case returnOperator input of
      Just operator -> do
        let (firstVal, restStack) = stackPop stack
        let (secondVal, restStack') = stackPop restStack
        calc (drop 1 input) (stackPush restStack' (operator firstVal secondVal))
      Nothing -> undefined

returnOperator :: (Num a, Fractional a) => String -> Maybe (a -> a -> a)
returnOperator input
  | input == "+" = Just (+)
  | input == "-" = Just (-)
  | input == "*" = Just (*)
  | input == "/" = Just (/)
  | otherwise = Nothing

-- case representsNumber nextValue of
--   True -> calc (tail input) (stackPush stack nextValue)
--   False -> undefined

infixr 5 :-:

data Stack a = EmptyStack | a :-: (Stack a) deriving (Show, Eq, Read, Ord)

testStack = stackPush (stackPush (makeStack 10) 2) 3

infixr 5 :.:

data List a = Empty | a :.: (List a) deriving (Show, Eq, Read, Ord)

testList = 2 :.: 3 :.: 4 :.: Empty

stackPush :: Stack a -> a -> Stack a
stackPush xs input = input :-: xs

stackPop :: Stack a -> (a, Stack a)
stackPop EmptyStack = undefined
stackPop (x :-: xs) = (x, xs)

-- class Stackable a where
--     pop :: [a] -> a
--     push :: [a] -> a -> [a]
-- instance Stackable Stack a where
--     push xs input = input:xs
--     pop (x:xs) = x

-- calc :: (Num a) => String -> [a] -> a
-- -- NOTE(pierre): head is unsafe, how else to solve it?
-- calc [] stack = head stack
-- calc input stack = do
--     let firstValue = head $ words input
--     case representsNumber of input
--         True -> calc (tail input) (stack:

-- calc (acc, numbers, operators, actions)
-- if order = PushStack then

-- MAYBE(pierre): where are functions for safeTail, safeHead etc..?
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x : xs) = xs

returnNumber :: (Num a, Read a) => String -> Maybe a
returnNumber input = if representsNumber input then Just $ read input else Nothing

representsNumber :: String -> Bool
representsNumber = all ((== True) . Char.isNumber)

data MyOperator = Addition | Multiplication | Division deriving (Show, Eq)

representsOperator :: String -> Bool
representsOperator input =
  let supportedOperators = words "+ - / *"
   in if (==) 1 $ length input then elem input supportedOperators else False

-- returnOperator :: String -> Maybe MyOperator
-- returnOperator input
--   | input == "+" = Just Addition
--   | input == "*" = Just Multiplication
--   | input == "/" = Just Division
--   | otherwise = Nothing

-- helperRPN :: (Num a, Num b, Fractional b) => String -> String -> Either a b
-- helperRPN input stack = do
--     case representsNumber input of
--         Just number -> number
--         Nothing -> -1
