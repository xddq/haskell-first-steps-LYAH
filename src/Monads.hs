-- src: http://learnyouahaskell.com/a-fistful-of-monads
-- monads are basicly just beefed up applicative functors. same as applicative
-- functors are just beefed up functors. just as functors are just beefed up
-- values. (this is not 100% correct, but a way to think of the hierarchy)

-- reminder:
-- - fmap: used for mapping functions over Functors (stuff that can be mapped over)
-- fmap :: (Functor f) => (a -> b) -> f a -> f b

-- - <*>: used for mapping Functors which contain functions over other functors.
-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
reminder1 = (*) <$> Just 2 <*> Just 8
reminder2 = (++) <$> Just "klingon" <*> Nothing
reminder3 = (-) <$> [3,4] <*> [1,2,3]

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

maybeAsFunctor1 = fmap (++"!") (Just "wisdom")
-- fmap over Nothing returns nothing since if we have multiple parameters and
-- one of them is nothing for normal values we have no clue how to handle. So
-- Nothing is a sane/expected default.
maybeAsFunctor2 = fmap (++"!") Nothing

-- all equal, one abstraction/syntactic sugar 'added' each time.
maybeAsApplicativeFunctor1 = fmap (+1) (Just 2)
maybeAsApplicativeFunctor2 = pure (+1) <*> Just 2
maybeAsApplicativeFunctor3 = (+1) <$> Just 2

maybeAsApplicativeFunctor4 = Just (+3) <*> Just 2
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
applyMaybe (Just x) f = Just (f x)
test1 = Just 3 `applyMaybe` (\x -> Just x+10)
-- TODO: continue here.
