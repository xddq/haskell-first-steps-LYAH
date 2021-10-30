-- src: http://learnyouahaskell.com/functionally-solving-problems
-- problem: RPN (reverse polish notation) --> calculating by putting onto stack
-- until we find an operator and then apply that operator to the (depending on
-- arity, mostly 2 I guess?) two values which are on the stack and we put the
-- result back on the stack. when we are on the end we pop the stack.
-- e.g.
-- RPN 10 4 3 + 2 * - --> 3+4=7;2*7=14;14-10=4; 4
import qualified Data.Char as Char

--
calcRPN :: (Num a) => String -> a
calcRPN input = do
    let inputArray = words input
    3

-- Returns True if all entries represent a number [0-9]. Else returns False.
-- E.g. "100" "39" --> True
representsNumber :: String -> Bool
representsNumber string = all (== True) $ map Char.isNumber string

helperRPN :: (Num a, Num b, Fractional b) => String -> String -> Either a b
helperRPN input stack
    | representsNumber $ input = Left $ read input
    | otherwise = Right $ returnOperator input


returnOperator :: (Num a, Fractional a) => String -> a -> a -> a
returnOperator input
    | input == "+" = (+)
    | input == "*" = (*)
    | input == "/" = (/)

