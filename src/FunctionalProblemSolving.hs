-- src: http://learnyouahaskell.com/functionally-solving-problems
-- problem: RPN (reverse polish notation) --> calculating by putting onto stack
-- until we find an operator and then apply that operator to the (depending on
-- arity, mostly 2 I guess?) two values which are on the stack and we put the
-- result back on the stack. when we are on the end we pop the stack.
-- e.g.
-- RPN 10 4 3 + 2 * - --> 3+4=7;2*7=14;14-10=4; 4
import qualified Data.Char as Char

testArray = "10 4 2 * -"

--
calcRPN :: (Num a) => String -> a
calcRPN input = do
  let numbers = filter representsNumber $ words input
  let operators = filter representsOperator $ words input
  let order = map representsNumber $ words input
  3

data Action = PushStack | PopStack deriving (Show, Eq)

-- basic minimal operation. if number -> put on stack, else pop from stack.
-- use tuple to return result and resulting list.
-- calc :: (Num a) => ([a], [MyOperator], [Action]) -> ([a], [MyOperator], [Action])
-- calc (xs, _, []) = (xs, _, [])
-- calc (numbers, operators, actions) = do
--     if head actions == PushStack then calc (xs

makeStack :: a -> Stack a
makeStack input = input :-: EmptyStack

calc :: [a] -> Stack a -> a
calc [] stack = result
    where (result, restStack) = stackPop stack
    -- TODO(pierre): continue here!

infixr 5 :-:
data Stack a = EmptyStack | a :-: (Stack a) deriving (Show, Eq, Read, Ord)
testStack = 2 :-: 3 :-: EmptyStack

infixr 5 :.:
data List a = Empty | a :.: (List a) deriving (Show, Eq, Read, Ord)
testList = 2 :.: 3 :.: 4 :.: Empty


stackPush :: Stack a -> a -> Stack a
stackPush xs input = input:-:xs

stackPop :: Stack a -> (a,Stack a)
stackPop EmptyStack = undefined
stackPop (x:-:xs) = (x, xs)

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

returnOperator :: String -> Maybe MyOperator
returnOperator input
  | input == "+" = Just Addition
  | input == "*" = Just Multiplication
  | input == "/" = Just Division
  | otherwise = Nothing

-- helperRPN :: (Num a, Num b, Fractional b) => String -> String -> Either a b
-- helperRPN input stack = do
--     case representsNumber input of
--         Just number -> number
--         Nothing -> -1
