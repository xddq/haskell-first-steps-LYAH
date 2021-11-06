-- src: http://learnyouahaskell.com/a-fistful-of-monads
-- monads are basicly just beefed up applicative functors. same as applicative
-- functors are just beefed up functors. just as functors are just beefed up
-- values. (this is not 100% correct, but a way to think of the hierarchy)
-- for MonadPlus, guards
import Control.Monad

-- reminder:
-- - fmap: used for mapping functions over Functors (stuff that can be mapped over)
-- fmap :: (Functor f) => (a -> b) -> f a -> f b

-- - <*>: used for mapping Functors which contain functions over other functors.
-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
reminder1 = (*) <$> Just 2 <*> Just 8

reminder2 = (++) <$> Just "klingon" <*> Nothing

reminder3 = (-) <$> [3, 4] <*> [1, 2, 3]

-- Main operation of Monads are 'bind'/(>>=). Type signature:
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-- - used when we have a boxed/contexted/fancy value and a function that takes a
-- normal value and returns a boxed value. Will then end up unwrapping the value
-- from our fancy value, then applying it and returning a boxed/contexted/fancy
-- value again.
-- TODO: does the monad have to be of the same type? Or do both just have to be
-- an instance of the monad typeclass?
--
-- NOTE: functors --> fmap function over boxed values
-- applicative functors --> fmap function over boxed values which also may be
-- functions.
-- monoids --> apply binary functions onto boxed values, fold up boxed values.
-- foldable --> enables folding over boxed values.

maybeAsFunctor1 = fmap (++ "!") (Just "wisdom")

-- fmap over Nothing returns nothing since if we have multiple parameters and
-- one of them is nothing for normal values we have no clue how to handle. So
-- Nothing is a sane/expected default.
maybeAsFunctor2 = fmap (++ "!") Nothing

-- all equal, one abstraction/syntactic sugar 'added' each time.
maybeAsApplicativeFunctor1 = fmap (+ 1) (Just 2)

maybeAsApplicativeFunctor2 = pure (+ 1) <*> Just 2

maybeAsApplicativeFunctor3 = (+ 1) <$> Just 2

maybeAsApplicativeFunctor4 = Just (+ 3) <*> Just 2

maybeAsApplicativeFunctor5 = Nothing <*> Just "greed"

-- >>= for Maybe.
-- takes a Maybe a, a function which takes a value and returns a Maybe b, and
-- returns a. this function will be appled to Maybe a.
-- NOTE: somehow this does not compile? idk.
-- instance Monad' Maybe where
--     >>=' Nothing _ = Nothing
--     >>=' (Just x) f = Just (f x)
-- TODO: this looks similar to functor for Maybe? How are they different?
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

test1 = Just 3 `applyMaybe` (\x -> Just $ x + 10)

test2 = Just "just" `applyMaybe` (\x -> Just $ x ++ " lose it")

test3 = Nothing `applyMaybe` (\x -> Just $ x ++ " lose it")

test4 = Just "just" `applyMaybe` (\x -> if (length x) > 5 then Just $ x ++ " longer" else Nothing)

test5 = Just "justtttt" `applyMaybe` (\x -> if (length x) > 5 then Just $ x ++ " longer" else Nothing)

-- Monad typeclass:
-- class Applicative m => Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b
--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y
--     fail :: String -> m a
--     fail msg = error msg
-- --> every monad is also an applicative. just beefed up :]
-- - return in Monad is the same as pure in Applicative --> takes a value and
-- puts box around it/puts it in context/wraps it inside monad

-- return for Maybe? --> wraps value in Just.
-- return for IO? --> does bogus/nothing IO and puts value in its box.

-- how Maybe is an instance of Monad
-- instance Monad Maybe where
--     return x = Just x
--     Nothing >>= _ = Nothing
--     Just x >>= f = f x
test6 = Just 5 >>= \x -> return $ x + 5

test7 = Nothing >>= \x -> return $ x + 5

-- WALK THE LINE, PIERRE
-- " Pierre has decided to take a break from his job at the fish farm and try
-- tightrope walking. He's not that bad at it, but he does have one problem:
-- birds keep landing on his balancing pole! They come and they take a short
-- rest, chat with their avian friends and then take off in search of
-- breadcrumbs. This wouldn't bother him so much if the number of birds on the
-- left side of the pole was always equal to the number of birds on the right
-- side. But sometimes, all the birds decide that they like one side better and
-- so they throw him off balance, which results in an embarrassing tumble for
-- Pierre (he's using a safety net).
--
-- Let's say that he keeps his balance if the number of birds on the left side of
-- the pole and on the right side of the pole is within three. So if there's one
-- bird on the right side and four birds on the left side, he's okay. But if a
-- fifth bird lands on the left side, then he loses his balance and takes a dive. "
type Birds = Int

type Pole = (Int, Int)

-- landLeft :: Pole -> Birds -> Pole
-- landLeft (left, right) x = (left + x, right)
-- landRight :: Pole -> Birds -> Pole
-- landRight (left, right) x = (left, right + x)
landLeft :: Birds -> Pole -> Pole
landLeft x (left, right) = (left + x, right)

landRight :: Birds -> Pole -> Pole
landRight x (left, right) = (left, right + x)

test8 = landLeft 1 (landRight 1 (landLeft 2 (0, 0)))

-- make a function to order to enable cleaner chaining.
x -: f = f x

test9 = 100 -: (* 3)

test10 = True -: not

test11 = (0, 0) -: landLeft 1 -: landRight 1 -: landLeft 2

-- adapt function. If abs x y > 3 then we return Nothing.
-- function that takes value and returns boxed one --> ding ding monad stuff
landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' x (left, right) =
  if abs (left + x - right) > 3
    then Nothing
    else Just (left + x, right)

landRight' :: Birds -> Pole -> Maybe Pole
landRight' x (left, right) = landLeft' x (right, left)

-- check if pierre falls down
test12 = landLeft' 4 (0, 0)

test13 = landLeft' 3 (0, 0)

-- so how to chain them now? we need Monad power?
-- following pairs of two are equal.
test14 = landLeft' 3 (0, 0) >>= landLeft' (-1) >>= landLeft' 1

test15 = return (0, 0) >>= landLeft' 3 >>= landLeft' (-1) >>= landLeft' 1

test16 = landLeft' 0 (0, 0) >>= landLeft' (-1) >>= landLeft' 1 >>= landRight' 10

test17 = return (0, 0) >>= landLeft' (-1) >>= landLeft' 1 >>= landRight' 10

-- introduce function which makes Pierre fall.
banana :: Pole -> Maybe Pole
banana _ = Nothing

test18 = return (0, 0) >>= landLeft' (1) >>= landRight' 2 >>= banana >>= landLeft' 1

-- "instead of coding banana with a function that takes any value and returns
-- the predetermined monadic value we can use >>."
-- (>>) :: (Monad m) => m a -> m b -> m b
-- m >> n = m >>= \_ -> n
-- --> we can replace banana with >> to introduce guaranteed failure!
-- TODO: why would we need/do this? XD It does take monad context into
-- consideration.. but still, why would we return guaranteed failure?
test19 = return (0, 0) >>= landLeft' (1) >>= landRight' 2 >> Nothing >>= landLeft' 1

-- >>= with Maybe is a classic example of computations that are based on
-- computations that might have failed.

-- do NOTATION
-- - since monads are used everywhere in Haskell they get their own notation.
-- Yes, it is the do that we did use for IO. It was used for glueing actions of
-- same type together.
-- The semantic has not changed. but it can be used for every type that is an
-- instance of the Monda typeclass!
reminder4 = Just 3 >>= (\x -> Just (show x ++ "!"))

-- TODO(pierre): try to understand the lambda inside of lambda stuff.
-- - first x is 3, then we get Just "!" where "!" will be fed into the second
-- lambda. so x is 3 and y is "!". Then we call show 3 ++ "!" which we get as
-- the result.
test20 = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

-- this is pretty similar to
test21 = let x = 3; y = "!" in show x ++ y

-- difference between 20 and 21 is that 20 consists of boxed/monadic values
-- which in this context contain a failure case.
-- failure cases:
-- TODO: why does this fail? Try typing it?
-- test22 = Nothing >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
test23 = Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))

test24 = Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing))

-- to make the similarity between the do notation clearer.. : ]
test25 :: Maybe String
test25 =
  Just 3
    >>= ( \x ->
            Just "!"
              >>= ( \y ->
                      Just (show x ++ y)
                  )
        )

-- and to avoid having to write tons of lambdas and bind them, haskell comes to
-- the rescue with do notation.
test26 :: Maybe String
test26 = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

-- --> takeaway: do notation/syntax is just syntactic sugar for chaining
-- (repeatedly binding/>>=) monadic values.
-- every line inside a do notation is a monadic value --> we have to use <- to
-- get the value of ouf the monadic box/context.
-- - the last line cannot use <- since it would not make sense. If we compare it
-- with the lambda and bind notation then it will be clear that the last one has
-- to be a monadic value with contains some values. (see test22,23,24)

-- pierres balance act can also be described in do notation.
routine :: Maybe Pole
routine = do
  start <- return (0, 0)
  first <- landLeft' 1 start
  second <- landRight' 2 first
  landLeft' 1 second

test27 = routine

-- throw banana in pierres routine
-- writine line without <- is just same as _ <- action
routine' :: Maybe Pole
routine' = do
  start <- return (0, 0)
  first <- landLeft' 1 start
  Nothing
  second <- landRight' 2 first
  landLeft' 1 second

test28 = routine'

justH :: Maybe Char
justH = do
  (x : xs) <- Just "hello"
  return x

-- default fail implementation for monads:
-- fail :: (Monad m) => String -> m a
-- fail msg = error msg
-- for monads which have context of possible failure (either, maybe) it often
-- gets reimplemented.
-- fail for maybe monad:
-- fail _ = Nothing

-- example of failure:
-- --> failure (unmatched pattern) causes a monad wide failure instead of a
-- program wide failure.
-- TODO: how could we adapt pattern matching here to not fail?
justFail :: Maybe Char
justFail = do
  (x : xs) <- Just ""
  return x

-- how list is a Monad
-- instance Monad [] where
--     return x = [x]
--     xs >>= f = concat (map f xs)
--     fail _ = []
-- NOTE: concat is similar to "flat" in js

-- what are non deterministic values? In the book he said values that can have
-- multiple values that are correct..? e.g. list with multiple 'possible' values
-- inside them?
-- --> using list monad we get all possible results for given data.
test29 = [3, 4, 5] >>= \x -> [x, - x]

-- for list monad failure is the empty list. [].
test30 = [] >>= \x -> [1, 2, 3]

test31 = [1, 2, 3] >>= \x -> []

-- NODE: when using list monad the computations can be seen as a tree with each
-- branch being fulfilled/used.
-- these all are equal.
test32 = [1, 2] >>= \num -> ['a', 'b'] >>= \ch -> return (num, ch)

-- list comprehensions is just syntactic sugar for >>=.
test33 = [(n, ch) | n <- [1, 2], ch <- ['a', 'b']]

-- do notation is just syntactic sugar for >>=.
test34 :: [(Int, Char)]
test34 = do
  -- n is 1 or 2
  n <- [1, 2]
  -- ch is a or b
  ch <- ['a', 'b']
  -- nondeterministic result will calculate each possible result.
  return (n, ch)

filterWithGuards1 = [1 .. 50] >>= \x -> guard ('7' `elem` show x) >> return x

-- filtering stuff with list comprehensions.
test35 = [n | n <- [1 .. 50], '7' `elem` show n]

-- how to do filtering with list monad in bind/>>= notation?

-- is for monads which can also act as monoid. (meaning they have mempty ->
-- producing the id for the monoid/monad in this case, and being able to apply a
-- binary function which takes two monoid values as arugments and returns a
-- monoid/ in this case monad)
-- all inside Control.Monad.
-- class Monad m => MonadPlus m where
--     mzero :: m a
--     mplus :: m a -> m a -> m a
-- instance MonadPlus [] where
--     mzero = []
--     mplus = (++)
-- mzero is failure value, mplus concats two list monadsplus.
-- instance MonadPlus (Maybe a) where
--     mzero = Nothing
--     mplus =
-- guard function.
-- guard :: (MonadPlus m) => Bool -> m ()
-- guard True = return ()
-- guard False = mzero

test36 :: Maybe ()
test36 = guard (5 > 2)

test37 :: [()]
test37 = guard (5 > 2)

test38 :: Maybe ()
test38 = guard (2 > 5)

-- filterWithGuards1 = [1 .. 50] >>= (\x -> guard ('7' `elem` show x) >> return x)

filterWithGuards2 = guard (5 > 2) >> return "nice" :: [String]

-- guard allows us to just evaluate later stuff in chain if we really need it.
-- if we get a failure value, just return the failure value of the given monad.
-- ([] for list, Nothing for Maybe, etc..)
-- TODO: how to chain guards??

-- KNIGHTS QUEST:
-- chessboard with tuples. first value being the row and second the column.
-- chessboard is 8x8 Check if the knight can reach the possition in three moves.
-- --> walk all possible moves three times and check if result is in our
-- resulting positions list.

type Position = (Int, Int)
type Moves = [Position]

-- moves the knight can do.
knightMoves :: [Position]
knightMoves = do
  let bigStep = [2, -2]
  let smallStep = [1, -1]
  let bigStepRow =
        bigStep >>= \row ->
          smallStep
            >>= \col ->
              return (row, col)
  let smallStepRow =
        smallStep >>= \row ->
          bigStep
            >>= \col ->
              return (row, col)
  bigStepRow ++ smallStepRow

makeMoves :: Position -> Moves -> [Position]
makeMoves (startRow, startCol) = map (\(row, col) -> (startRow + row, startCol + col))

reachablePositions :: Position -> [Position]
reachablePositions start = do
    first <- makeMoves start knightMoves
    second <- makeMoves first knightMoves
    third <- makeMoves second knightMoves
    filter inBoard [first, second, third]

canReach :: Position -> Position -> Bool
canReach start end = end `elem` reachablePositions start

-- checks if position is in chessboard (row 1-8, col 1-8)
inBoard :: Position -> Bool
inBoard (row, col) = all (== True) $ [(>= 1), (<= 8)] <*> [row, col]


-- SOLUTION
--
