-- src: http://learnyouahaskell.com/for-a-few-monads-more
-- remider:
-- - Maybe Monad --> adds context of possible failure
-- - List Monad --> adds context of non-deterministic
-- used for Sum type.

-- used for Writer Monad.

-- used for WriterT typedef

-- used for random and StdGen

-- used for state Monad
import Control.Monad.State
import Control.Monad.Writer
-- for error section
import Data.Functor.Identity
import Data.Monoid
import System.Random

-- Writer Monad --> values which have another value attached which will be
-- combined into one log.

-- checks if gang is big.
isBigGang' :: Int -> Bool
isBigGang' = (> 9)

-- checks if gang is big and we also want to notify/log result as string
isBigGang :: Int -> (Bool, String)
isBigGang x = ((< 9) x, "Compared gang size to 9.")

testGang1 = isBigGang 3

testGang2 = isBigGang 10

-- ensure that string is appended.
-- applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog (val, log) f = (newVal, log ++ msg)
  where
    (newVal, msg) = f val

-- test with (Int, String)
testApplyLog = applyLog (2, "hello") (\x -> (x + 2, " world"))

testApplyLog1 = (30, "A freaking platoon.") `applyLog` isBigGang

testApplyLog2 = (3, "Smallish gang.") `applyLog` isBigGang

-- test with (String, String)
testApplyLog3 = ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))

-- applyLog to include lists of any type beside just chars.
applyLog' :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog' (val, log) f = (newVal, log ++ msg)
  where
    (newVal, msg) = f val

-- applyLog to include monoids.
-- TODO: why do we have to write m instead of m c? Monoid is also a box/context,
-- no?
-- applyLog'' :: (Monoid m) => (a, m c) -> (a -> (b, m c)) -> (b, m c)
applyLog'' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog'' (val, log) f = (newVal, log `mappend` msg)
  where
    (newVal, msg) = f val

-- "Because the accompanying value can now be any monoid value, we no longer have
-- to think of the tuple as a value and a log, but now we can think of it as a
-- value with an accompanying monoid value. For instance, we can have a tuple that
-- has an item name and an item price as the monoid value. We just use the Sum
-- newtype to make sure that the prices get added as we operate with the items.
-- Here's a function that adds drink to some cowboy food: "
type Food = String

type Drink = String

type Price = Sum Int

addDrink :: Food -> (Drink, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

-- NOTE: Sum newtype just makes sure we add stuff together using a monoid.
testAddDrink1 = ("I eat nuffin", Sum 0) `applyLog''` addDrink

-- beans cost 20x. cause we drink milk with it we get Sum 20 `mappend` Sum 25 =
-- Sum 45.
testAddDrink2 = ("beans", Sum 20) `applyLog''` addDrink `applyLog''` addDrink

-- --> so we can build a reader Monad which could log messages, add up costs or
-- do whatever the characteristic of the given Monoid is.

-- WRITER Monad
-- we have just seen what a value with an attached Monoid value can be used for.
-- Now lets explore what can happen with a value with an attached Monad.
-- first step. wrap tuple in newtype so it can easily be made an instance of
-- Monad and is different from the default tuple type.
-- newtype Writer w a = Writer { runWriter :: (a, w) }
-- how Writer is a Monad:
-- instance (Monoid w) => Monad (Writer w) where
--     return x = Writer (x, mempty)
--     (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
-- expect default identity values inside the second parameter of our resulting
-- writer.
-- id = ""
testWriter1 = runWriter (return 3 :: Writer String Int)

-- id = Sum 0
testWriter2 = runWriter (return 3 :: Writer (Sum Int) Int)

-- id = Product 1
testWriter3 = runWriter (return 3 :: Writer (Product Int) Int)

-- Writer with do notation.
-- TODO: why does this copied example not work? Did something in prelude /base
-- change?
-- looks like from mtl 1.X to mtl 2.X this changed. We have to create writer
-- using the writer function instead of the Writer newtype.
-- src: https://stackoverflow.com/questions/11684321/how-to-play-with-control-monad-writer-in-haskell/11684566#11684566
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog' :: Writer [String] Int
multWithLog' = do
  a <- logNumber 3
  b <- logNumber 5
  return (a * b)

-- we can use some functions to just target the monoid value of our writer using
-- tell.
multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["We will multiply both values."]
  return (a * b)

-- adding logging to problems:
-- Euclidic algorithm to get the gcd="greatest common divisor"/ggt="größter
-- gemeinsamer Teiler"
gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)

-- add logging by using list of strings as our monoid.
-- NOTE: here again we had to adapt the type definition to match the mtl 2.X
gcd'' :: Int -> Int -> WriterT [String] Identity Int
gcd'' a b
  | b == 0 = do
    -- can use tell here because we are inside the Writer monad within the
    -- do block.
    tell ["Finished with: " ++ show b]
    return b
  | otherwise = do
    -- can use tell here because we are inside the Writer monad within the
    -- do block.
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd'' b (a `mod` b)

-- gcd'' with logging, using the writerT monad.
testGcd :: WriterT [String] Identity Int
testGcd = gcd'' 8 3

-- display the [String] monoid using mapM_ and putStrLn.
printResultsGcd :: IO ()
-- reminder: mapM_ takes function which takes a value and returns a
-- monad/context/boxed value. also takes a foldable/can be mapped over data
-- structure where it gets the value where it applies the first function on
-- from. Returns the resulting monad with ()/void/nothing.
printResultsGcd = mapM_ putStrLn $ snd $ runWriter testGcd

-- INEFFICIENT LIST CONSTRUCTION
-- reminder:
-- 1) xs ++ x goes through every element in xs and prepends them one by one to x.
-- 2) x ++ xs takes x and prepends it the first elem of xs.
-- examples:
-- in our gcd'' function it logs like this:
-- a ++ (b ++ (c ++ (d ++ (e ++ f))))
-- but if we changed the order of tell to be after the recursive call, we would
-- end up with something like (in our gcdReverse function)
-- ((((a ++ b) ++ c) ++ d) ++ e) ++ f
-- which is way, way worse / takes way longer to calculate.
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result

-- DIFFERENCE LISTS
-- can be used instead of lists when we have repeated appending of lists.
-- - function that takes a list and prepends another list to it.
-- appending with function and using composition:
-- create infix append definition
f `append` g = \xs -> f (g xs)

-- --> no matter how often we append, we only use the small junk we are starting
-- with. And we don't have to iterate through given lists multiple times. Just
-- once.
diffListApproach1 = ("dog" ++) `append` ("meat" ++)

diffListApproach2 = ("dog" ++) `append` ("meat" ++) `append` ("hello" ++)

diffListApproach3 = diffListApproach2 "world"

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

diffListappend :: DiffList a -> DiffList a -> DiffList a
(DiffList f) `diffListappend` (DiffList g) = (DiffList (\xs -> f (g xs)))

-- NOTE: have to write it like this because the mappend moved from the Monoid
-- typeclass to the semigroup. (<>) and mappend seem to the same operation.
-- https://stackoverflow.com/questions/53622428/a-basic-monoid-definition-gives-no-instance-for-semigroup-mymonoid-arising-fr
-- TODO: what is a semigroup? How/When to use it?
instance Semigroup (DiffList a) where
  (<>) = (diffListappend)

instance Monoid (DiffList a) where
  mempty = toDiffList []

-- mappend = (diffListappend)

-- <> is the same as `mappend`
testDiffList1 = fromDiffList $ toDiffList [1, 2, 3] <> toDiffList [4, 5, 6]

testDiffList2 = fromDiffList $ toDiffList [1, 2, 3] `mappend` toDiffList [4, 5, 6]

-- apply diffLists to gcdReverse to avoid the right associativity problem
-- (concat longer and longer lists with small lists multiple times.) see above
-- when gcdReverse was defined.
gcdReverseDiffList :: Int -> Int -> Writer (DiffList String) Int
gcdReverseDiffList a b
  | b == 0 = do
    tell $ toDiffList ["Finished with " ++ show a]
    return a
  | otherwise = do
    result <- gcdReverseDiffList b (a `mod` b)
    tell $ toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result

testGcdReverseDiffList =
  mapM_ putStrLn . fromDiffList . snd . runWriter $
    gcdReverseDiffList 110 34

-- comparing performance between diffList (fixes right associativity problem)
-- and a right associatively built/constructed list.
-- Counts from 100 to 0 but tells/adds/concats/ log in reverse order. (resulting
-- in right assoc list concat) see info above at gcdReverse.
countdownRightAssoc :: Int -> Writer [String] Int
countdownRightAssoc x
  | x == 0 = do
    tell ["Finished!"]
    return x
  | otherwise = do
    result <- countdownRightAssoc (x -1)
    tell ["At " ++ show x]
    return result

countdownRightAssocDiffList :: Int -> Writer (DiffList String) Int
countdownRightAssocDiffList x
  | x == 0 = do
    tell $ toDiffList ["Finished!"]
    return x
  | otherwise = do
    result <- countdownRightAssocDiffList (x -1)
    tell $ toDiffList ["At " ++ show x]
    return result

-- SOLUTION: (shorter, better since we are just using tell. and not retuning
-- values)
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x -1)
  tell (toDiffList [show x])

-- NOTE: if you really don't see a difference in speed just increase the value
-- given here.
testCountdownRightAssoc = mapM_ putStrLn . snd . runWriter $ countdownRightAssoc 5000

testCountdownRightAssocDiffList = mapM_ putStrLn . fromDiffList . snd . runWriter $ countdownRightAssocDiffList 5000

-- READER Monad
-- --> about combining functions with Monads.
-- reminder:
-- results in (x+3) * 5
-- same as (*5) . (+3)
reminderFunctorFuncComp1 x = fmap (* 5) (+ 3) x

reminderFunctorFuncComp2 x = (* 5) . (+ 3) $ x

-- results in \x -> (x*2) + (x+5)
reminderFunctorFuncComp3 x = (+) <$> (* 2) <*> (+ 5) $ x

-- how functions are Monads
-- instance Monad ((->) r) where
--    return a = \_ -> a
--    h >>= f = \w -> f (h w) w
-- TODO: try to get this. Start with examples first for now.
-- "The implementation for >>= seems a bit cryptic, but it's really not all that.
-- When we use >>= to feed a monadic value to a function, the result is always a
-- monadic value. So in this case, when we feed a function to another function, the
-- result is a function as well. That's why the result starts off as a lambda. All
-- of the implementations of >>= so far always somehow isolated the result from the
-- monadic value and then applied the function f to that result. The same thing
-- happens here. To get the result from a function, we have to apply it to
-- something, which is why we do (h w) here to get the result from the function and
-- then we apply f to that. f returns a monadic value, which is a function in our
-- case, so we apply it to w as well. "

-- can use do expression for Int -> Int since it equal to (Int -> Int) and
-- functions are Monads.
-- these two are equal in their result.
-- using the function Monad
addStuff :: (Int -> Int)
addStuff = do
  a <- (* 2)
  b <- (+ 10)
  return (a + b)

-- using the function applicative Functor
addStuff' = (+) <$> (* 2) <*> (+ 10)

testAddStuff1 = addStuff 4

testAddStuff2 = addStuff' 4

-- the function Monad is also called the Reader Monad because every function
-- reads from the same source.
-- another addStuff implementation to show that a bit clearer.
addStuff'' :: (Int -> Int)
addStuff'' x =
  let a = (* 2) x
      b = (+ 10) x
   in (a + b)

-- NOTE: reader/function Monad is useful when we have a lot of functions which
-- all take one parameter and eventually will be applied to the same thing. e.g.
-- see addStuff.
testAddStuff3 = addStuff'' 4

-- STATEFUL COMPUTATIONS
-- reminder: when generating random numbers in an older section we used:
-- to get an initial random generator which then generates a new generator which
-- then has to be called in the second iteration etc..
-- REASON: since haskell is a pure function programming lanugage a function with
-- the same parameters always yields the same results. Therefore we have to
-- generate new parameters/functions when getting random stuff.
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
   in (firstCoin, secondCoin, thirdCoin)

-- but state Monad comes to the rescue.
-- lets define a stateful computation as a computation that takes a value of
-- type state and returns a result and a new value of type state.
-- s -> (a,s)

-- lets model a stack for displaying stateful stuff.
type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x : xs) = (x, xs)

-- TODO: find out which order of the params is better and find a generic rule,
-- if there is one. [Stack] -> Int or Int -> [Stack] in this example.
push :: Stack -> Int -> ((), Stack)
push xs x = ((), xs)

-- does not make sense. just some random stack stuff. push 3, pop, pop on any
-- given stack.
doStackStuff :: Stack -> (Int, Stack)
doStackStuff xs =
  let ((), newStack) = push xs 3
      (a, newStack') = pop newStack
   in pop newStack'

-- goal: do someting like this instead..
-- doStackStuff = do
--     push 3
--     a <- pop
--     pop
-- the STATE MONAD comes to the rescue.
-- remember the definition of what we think a function that manages state would
-- look like.
-- newtype State s a = State { getState :: s -> (a,s) }
-- NOTE: would make instanec of applicative as well since applicative is now
-- part of Monad typeclass. Skip this for now.
-- how state is a Monad
-- instance Monad (State s) where
--     return x = State $ \s -> (x,s)
--     -- (State f) >>= \s -> (a,s) = State $ \s -> (f a, f s)
--     -- SOLUTION:
--     (State h) >>= f = State $ \s -> let (a, newState) = h s
--                                         (State g) = f a
--                                     in g newState
-- using state monad with out pop and push stack functions.
-- value will be the result, state will be the list/our stack.
pop' :: State Stack Int
pop' = state $ \(x : xs) -> (x, xs)

push' :: Int -> State Stack ()
push' a = state $ \xs -> ((), a : xs)

-- had to use StateT here since something in the implementation did change since
-- the LYAH book. can use state instead of State value constructor to create a
-- State. Can use the StateT Monad instead of State.
doStackStuff' :: StateT Stack Identity Int
doStackStuff' = do
  push' 3
  pop'
  -- a <- pop'
  -- push' a
  pop'

testStateMonadWithStack1 = runState doStackStuff' [1, 2, 3]

-- "what if we want to do this: pop one number off the stack and then if that
-- number is 5 we just put it back onto the stack and stop but if it isn't 5, we
-- push 3 and 8 back on? Well, here's the code: "
-- NOTE: how would we write the types if it could return push or pop?? Would we
-- write a function like that in haskell?
doCondStackStuff' :: StateT Stack Identity ()
doCondStackStuff' = do
  a <- pop'
  if a == 5
    then do
      push' a
    else do
      push' 3
      push' 8

-- no 5? push 3 and 8 on stack after pop first.
testStateMonadWithStack2 = runState doCondStackStuff' [1, 2, 3]

-- since it is 5, push back on stack.
testStateMonadWithStack3 = runState doCondStackStuff' [5, 1, 2, 3]

-- combine our State Identity () and State Identity Int functions into one
-- function. Can be done since both operate on the State Monad.
combineStackStuff :: StateT Stack Identity ()
combineStackStuff = do
  a <- doStackStuff'
  if a == 5
    then doCondStackStuff'
    else return ()

testStateMonadWithStack4 = runState combineStackStuff [1, 2, 3]

testStateMonadWithStack5 = runState combineStackStuff [5, 2, 3]

testStateMonadWithStack6 = runState combineStackStuff [5, 5, 2, 3]

-- from Control.Monad.State we get the MonadState typeclass which delivers us
-- useful functions to set and set state specifically.
-- get = State $ \s -> (s,s)
-- put newState = State $ \s -> ((),newState)
getPutStacks :: StateT Stack Identity ()
getPutStacks = do
  stack <- get
  if stack == [1, 2, 3]
    then put $ map (* 2) stack
    else put $ map (+ 10) stack

testStateMonadWithStack7 = runState getPutStacks [1, 2, 3]

testStateMonadWithStack8 = runState getPutStacks [4, 4]

-- what >>= what would like if only working for States:
-- (>>=) :: State s a -> (a -> State s b) -> State s b
-- "See how the type of the state s stays the same but the type of the result
-- can change from a to b? This means that we can glue together several stateful
-- computations whose results are of different types but the type of the state
-- has to stay the same. Now why is that? Well, for instance, for Maybe, >>= has
-- this type: "
-- --> meaning here that e.g. we could have State of Stacks with a return value of Int, but also have a return value of String or (). See getPutStacks and doStackStuff for example of this.
-- --> the monad itself doesn't change. else we could not bind/>>= them. We can
-- still change the value of the State because we did make (State s) a Monad.
-- E.g. Maybe Monad will also stay Maybe, but may result in another type of
-- value.
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

-- Calculate random numbers using the state Monad.
-- TODO: check out State Monad again and make sure to understand how 1:1 this
-- definition fits with out StdGen case. Currently unclear.
-- - Nice to understand with the push' and pop' code. Our state takes a rndGen
-- and returns a (result, newStdGen).
-- Pretty mind boggling for now. Check State Monad definition again. I think every time
-- we have a function which is like s -> (a,s) we can use it. since random is:
-- random :: (Random a, RandomGen g) => g -> (a, g)
-- it is a perfect fit.
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

doRandomNumbers :: State StdGen (Bool, Bool, Bool)
doRandomNumbers = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

testRandomState1 = runState doRandomNumbers (mkStdGen 1)

-- --> State Monad enables us to do computations which need to store some kind
-- of state in between steps in a clean way.

-- ERROR HANDLING
-- Until now we have learned about Maybe being used for computations which might
-- fail. We did only shortly (if at all) look at Either e a. (e for error, a for
-- value of any type. just convention, but it is Either a b where both a and b
-- can be a value of any type.)
-- reminder for either, check their types :]
-- - basicly either is a maybe with a context for the type of failure (maybe
-- only shows that a failure happened, using Nothing)
testEither1 = Right 4

testEither2 = Left "Out of monads error O_O!"

-- NOTE: Either e is the Monad with the type of e being fixed to error.
-- --> Returning Right values of Either may be of different types. Just
-- - e type is for types that imlement the strMsg function. E.g. String !
-- Left/Error here is of type error!
-- instance (Error e) => Monad (Either e) where
--     return x = Right x
--     Right x >>= f = f x
--     Left err >>= f = Left err
--     fail msg = Left (strMsg msg)
-- NOTE: strMsg seems to be deprecated now. Since it was only a small example
-- and does not seem to be relevant since we mostly use String (I guess) skip
-- these two lines.
testEither3 = Left "boom" >>= \x -> return (x + 1)

testEither4 = Right 100 >>= \x -> Left "no way!"

testEither5 = Right 100 >>= \x -> return (x + 200)

-- REIMPLEMENT POLE WALKING with Either as Error Monad.
-- TODO: Can we reimplement this to also use the state monad on
-- top of the Either Monad? We would need to build a State where the value is of
-- type Either String Int and the State is of type Pole/(Int,Int)? And stop with
-- calculating when the value is a Left?
type Birds = Int

type Pole = (Int, Int)

landLeft :: Birds -> Pole -> Either String Pole
landLeft x (left, right) =
  if abs (newLeft - right) > 3
    then
      Left $
        "To many birds! Pierre is falling down! Left: " ++ show newLeft
          ++ " right: "
          ++ show right
    else Right (left + x, right)
  where
    newLeft = (left + x)

landRight :: Birds -> Pole -> Either String Pole
landRight x (left, right) = landLeft x (right, left)

routine1 :: Either String Pole
routine1 = do
  start <- return (0, 0)
  first <- landLeft 1 start
  second <- landRight 2 first
  landLeft 1 second

routine2 :: Either String Pole
routine2 = do
  start <- return (0, 0)
  first <- landLeft 1 start
  second <- landRight 2 first
  third <- landLeft 5 second
  -- NOTE: if after this line some intense computations were listed they would
  -- not even be ran, since we already had a Left before this.
  landRight 1 third

-- SOME USEFUL MONADIC FUNCTIONS
-- --> meaning functions that either operate on monadic values or return monadic
-- values.
-- - every monad is an applicative functor, every applicative functor is a
-- monad.
-- NOTE: nowadays Applicative is required for Monad. Do people use fmap for
-- Monads of liftM?
-- --> use fmap for everything. forget liftM.
-- src: https://www.reddit.com/r/haskell/comments/qow9za/lyah_beginner_questions/
-- liftM :: (Monad m) => (a -> b) -> m a -> m b
-- fmap :: (Functor f) => (a -> b) -> f a -> f b
-- don't even learn about liftM and ap. use fmap and <*> instead. (every monad is
-- nowadays also defined as an applicative.)

-- liftA2
-- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
-- short notation for wrapping function into functor context, and applying it to
-- both functors. returning the functor containing the result of the binary
-- function and applied to both functors.
-- liftA2 f x y = f <$> x <*> y

-- JOIN
-- used to flatten monadic values. e.g. join Just (Just 10) --> Just 10
-- join :: (Monad m) => m (m a) -> m a
-- TODO: can the nested monads also be of different type? e.g. state monad and
-- maybe monad? Which one will be the result? the inner or the outer?
-- --> seems like monads have to be of same type !!! either monads nested, maybe monads
-- nested etc.. 'monad combinators' should be the solution I think. but study
-- after finish LYAH book.

-- joining/flattening lists
-- calls mappend on inner monads --> [1,2,3] `mappend` [4,5,6]
testJoinLists = join [[1, 2, 3], [4, 5, 6]]

-- joining writer monads
-- --> calls mappend from value of outer monad with the value of the inner
-- monad.
testJoinWriter = runWriter $ join (writer (writer (1, "aaa"), "bbb"))

-- flattening/joining Either monad
testJoinEather = join (Right (Right 9))

-- flattening/joining State monad
testJoinState = runState (join (state $ \s -> (push' 10, 1 : 2 : s))) [0, 0, 0]

-- TODO: how does this work? join somehow joins the result of the function for
-- the current state with the new state?? Isn't this rather confusing rather
-- than helpful?
-- implementation of join:
join' :: (Monad m) => m (m a) -> m a
join' mm = do
  m <- mm
  m

-- example with Maybe monad:
joinedMaybes :: Maybe Int
joinedMaybes = do
  m <- Just (Just 8)
  m

-- m >>= f is always the same thing as join (fmap f m)
boundMaybes1 :: Maybe Int
boundMaybes1 = Just 3 >>= \x -> Just $ x + 1

-- NOTE: fmap has this type:
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- but still the a or b can be monads with values themselves. this is what we
-- are using here.
boundMaybes2 :: Maybe Int
boundMaybes2 = join $ fmap (\x -> Just (x + 1)) (Just 3)

-- NOTE:
-- this equality can be used to implement >>=. Since it is easier to figure out
-- how to flatten a monadic value than figuring out how to implement >>=.
-- TODO: ...okay? Why should this be the case?

-- filterM
-- xD filterM does filter any applicative value (I think it was kept filterM for
-- backwards compatibility when applicative was moved into monad typeclass :D)
reminderFilter1 = filter (\x -> x < 4) [1 .. 10]

-- "Now, let's make a predicate that, aside from presenting a
-- True or False result, also provides a log of what it did. Of course, we'll be
-- using the Writer monad for this: "
-- NOTE: this 16 lines of code were just testing, can ignore.
filterStuff :: [a] -> Writer [String] [a]
filterStuff xs = writer (xs, ["Did filter stuff!"])

filterWithLog :: Num a => Writer [String] [a]
filterWithLog = do
  a <- filterStuff [1, 2, 3]
  b <- filterStuff [4, 5, 6]
  return (a ++ b)

-- predicateWriter :: (a -> Bool) -> Writer [String] Bool
-- predicateWriter f = writer (f, ["did filter stuff!"])
--
-- filterWithLog' :: Writer [String] Bool
-- filterWithLog' = do
--     a <- filter
-- what to do here? call filterM? but we did not have filterM yet..?
-- SOLUTION:
filterWithLog' :: Int -> Writer [String] Bool
filterWithLog' x
  | x < 4 = do
    tell $ ["Keeping number: " ++ show x]
    return True
  | otherwise = do
    tell $ ["Throwing number: " ++ show x ++ " away. To big."]
    return False

testWriterPredicate1 = runWriter $ filterWithLog' 1

testWriterPredicate2 = runWriter $ filterWithLog' 2

testWriterPredicate3 = runWriter $ filterWithLog' 5

-- lets throw this predicate (which returns a monadic value) to filterM!
testFilterM1 = filterM filterWithLog' [1 .. 10]

-- print the log of the calculation
testFilterM2 = mapM_ putStrLn . snd . runWriter $ filterM filterWithLog' [9, 4, 30, 2, 1, 10, 3]

-- print the result of the calculation
testFilterM3 = fst . runWriter $ filterM filterWithLog' [9, 4, 30, 2, 1, 10, 3]

-- can get powerset (all permutations I think?) of a list (if we think of a list
-- as a set :D) using filterM
-- using behaviour of list monad (nondeterminism) to create a powerset.
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

-- TODO: try to understand this. I think it is fairly easy if I get my ahead
-- around the non-determinism of the list monad when checking this out.
testPowerSet1 = powerset [1, 2, 3]

testPowerSet2 = powerset [4, 5]

-- FOLDM
-- foldM
-- --> folding on monads.
-- Takes a function with acc and x. uses acc and x to create a Monad.
-- --> I think it is basicly foldl but the result will be wrapped in a Monad.
-- type signatures:
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
testFoldL1 = foldl (\acc x -> acc + x) 0 [1, 2, 3]

testFoldM1 = foldM (\acc x -> Just (acc + x)) 0 [1, 2, 3]

testFoldM2 = foldM (\acc x -> if even x then Just (acc + x) else Nothing) 0 [1, 2, 3]

testFoldM3 = foldM (\acc x -> if even x then Just (acc + x) else Nothing) 0 [2, 4 .. 20]

-- folding with binary function which returns writer monad:
foldingFunction :: Int -> Int -> Writer [String] Int
foldingFunction acc x = do
  let result = acc + x
  tell $ ["Current accumulator: " ++ show acc ++ " current x: " ++ show x ++ " result: " ++ show result]
  return result

-- returns the writer Monad
testFoldM4 = foldM foldingFunction 0 [1 .. 20]

-- prints log as result
testFoldM5 = mapM_ putStrLn . snd . runWriter $ foldM foldingFunction 0 [1 .. 20]

-- SAFE RPN CALCULATOR
-- remember the RPN calculator from src: http://learnyouahaskell.com/functionally-solving-problems#reverse-polish-notation-calculator
-- in file: FunctionalProblemSolving.hs
-- "
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

-- "
-- adapt this to be failsafe :]
-- TODO: why does reads not work? Probably changed the function again.. But this
-- time cannot find the correct version right now. Skip this for now. Not to
-- important.
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
  [(x, "")] -> Just x
  _ -> Nothing

-- MAYBE: do RPN failsafe.

-- COMPOSING MONADIC FUNCTIONS:
-- can compose monadic functions using <=<
-- same as . but for functions of type a -> m b
-- reminder, normal function composition
monadicComposition1 = let f = (+ 1) . (* 100) in f 4

-- could also use Just instead of return but return is more generic and since we
-- are feeding it a Just 4 haskell can infer it will become the type of the
-- Maybe Monad
monadicComposition2 = Just 4 >>= f
  where
    f = (\x -> return (x + 1)) <=< (\x -> return (x * 100))

-- can compose bunch of functions in a list using foldl and .
monadicComposition3 = let f = foldr (.) id [(+ 1), (* 100), (+ 1)] in f 1

-- can compose monadic functions more or less in the same way. just using <=<
-- instead of . and return instead of id.
-- TODO: why can we use foldl here?
monadicComposition4 = let f = foldl (<=<) return [(\x -> Just $ x + 1), (\x -> Just $ x * 100), (\x -> Just $ x + 1)] in f 1

-- monadic composition for the moveKnights problem.
-- first, the

-- copy paste from knights quest from Monads.hs:
-- SOLUTION
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <-
    [ (c + 2, r -1),
      (c + 2, r + 1),
      (c -2, r -1),
      (c -2, r + 1),
      (c + 1, r -2),
      (c + 1, r + 2),
      (c -1, r -2),
      (c -1, r + 2)
      ]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- new code:
replicateTest = replicate 3 (\x -> x + 1)

replicateTest1 = map (\x -> x 1) replicateTest

replicateTest2 = replicate 3 moveKnight

replicateTest3 = map (\f -> f (1, 1)) replicateTest2

-- run moveKnight x times with monadic function composition
replicateTest4 = foldl (<=<) return replicateTest2

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inX start
  where
    inX = foldl (<=<) return $ replicate x moveKnight

-- TODO: continue at Making Monads.
