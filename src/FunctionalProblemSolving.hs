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

calcRPN :: String -> IO String
calcRPN input = do
  putStrLn "hello"
  return "hello"

main = do
  putStrLn "Enter your expression in RPN (reverse polish notation)."
  putStrLn "Example: 10 4 3 + 2 * - "
  input <- getLine
  putStrLn $ "input was: " ++ input
  let result = show $ calc input EmptyStack
  putStrLn $ "result is: " ++ result

-- MAYBE(pierre): why cant I use this with interact?
-- putStrLn "Enter your expression in RPN (reverse polish notation)."
-- putStr "Example: 10 4 3 + 2 * - "
-- interact (\input -> show input)

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
