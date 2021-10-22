-- INPUT AND OUTPUT
-- src: http://learnyouahaskell.com/input-and-output

-- haskell is a purely functional programming language --> functions should not
-- have side effects (e.g. change state of a variable).
-- BUT we need to have IO / side effects. We need to react to user input, and
-- write to output for a language to not be useless :]

-- SOLUTION:
-- - separate code that is pure and unpure.
-- LETS DO IO FOR THE FIRST TIME <33
main = putStrLn "hello world"
