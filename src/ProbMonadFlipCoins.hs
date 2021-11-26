-- Prob value constructor is of type [(a, Rational)]
-- we can make an instance using Prob [(1,1%3)], or Prob [("hello",1%4)]
newtype Prob a =
  Prob
    { getProb :: [(a, Rational)]
    }
  deriving (Show, Eq)

data Coin
  = Heads
  | Tails
  deriving (Show, Eq)

thisSituation :: Prob (Prob Char)
thisSituation =
  Prob
    [ (Prob [('a', 1 % 2), ('b', 1 % 2)], 1 % 4)
    , (Prob [('c', 1 % 2), ('d', 1 % 2)], 3 % 4)
    ]

flattenProbs :: Prob (Prob a) -> Prob a
flattenProbs (Prob xs) = Prob $ concat $ map multProbs xs
  where
    multProbs (Prob xsInner, probOuter) =
      map (\(val, probInner) -> (val, probInner * probOuter)) xsInner

-- same as above, without helper function where.
flattenProbs2 :: Prob (Prob a) -> Prob a
flattenProbs2 (Prob xs) =
  Prob $
  concat $
  map
    (\(Prob xsInner, probOuter) ->
       map (\(val, probInner) -> (val, probInner * probOuter)) xsInner)
    xs

testFlattenProbs = flattenProbs thisSituation

-- fmap :: (a -> b) -> f a -> f b
instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(val, prob) -> (f val, prob)) xs

instance Applicative Prob where
  pure x = Prob [(x, 1 % 1)]
  (Prob fs) <*> (Prob xs) =
    Prob ([(f x, ratio) | (f, _) <- fs, (x, ratio) <- xs])

instance Monad Prob where
  return x = pure x
  m >>= f = flattenProbs (fmap f m)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

flipCoins :: Prob Bool
flipCoins = do
  a <- coin
  b <- coin
  c <- loadedCoin
  -- goes through every possibility from a, every from b and every from c.
  -- for each function coin of loadedCoin we get two results. ending up in 2 * 2
  -- * 2 = 8 possible results. See examples below.
  return (all (== Tails) [a, b, c])

-- Prop xs is the same as Prob {getProb = xs}
probGetProbXs = Prob {getProb = [(Heads, 1 % 2), (Tails, 1 % 2)]}

-- this means coin results in this. A Prob with a list of tuples of type (coin,
-- rational)
probXs = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

-- 0 1 2 are the same, just without syntactic sugar
flipCoins0 :: Prob Coin
flipCoins0 = do
  a <- coin
  return a

flipCoins1 :: Prob Coin
flipCoins1 = coin >>= (\a -> return a)

flipCoins2 :: Prob Coin
flipCoins2 = flattenProbs $ fmap (\a -> return a) coin

-- 3 4 5 6 are the same, just without syntactic sugar.
flipCoins3 :: Prob Bool
flipCoins3 = do
  a <- coin
  return (all (== Tails) [a])

flipCoins4 :: Prob Bool
flipCoins4 = coin >>= (\a -> return (all (== Tails) [a]))

flipCoins5 :: Prob Bool
flipCoins5 = flattenProbs $ fmap (\a -> return (all (== Tails) [a])) coin

-- same as 5 but with Prob constructor instead of coin function
flipCoins6 :: Prob Bool
flipCoins6 =
  flattenProbs $
  fmap (\a -> pure (all (== Tails) [a])) $ Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

-- equal to 6, not the "same". This shows the functions in which fmap f xs in flipCoins6 will result.
flipCoins6Example :: Prob Bool
flipCoins6Example =
  Prob $
  [ ((\a -> all (== Tails) [a]) Heads, 1 % 2 :: Rational)
  , ((\a -> all (== Tails) [a]) Tails, 1 % 2 :: Rational)
  ]

-- 7 8 9 are the same, just without syntactic sugar.
flipCoins7 :: Prob Bool
flipCoins7 = do
  a <- coin
  b <- coin
  return (all (== Tails) [a, b])

flipCoins8 :: Prob Bool
flipCoins8 = coin >>= (\a -> coin >>= (\b -> return (all (== Tails) [a, b])))

flipCoins9 :: Prob Bool
flipCoins9 =
  flattenProbs $
  fmap
    (\a -> flattenProbs $ fmap (\b -> return (all (== Tails) [a, b])) coin)
    coin

sumProbs' :: Prob Bool -> Prob Bool
sumProbs' (Prob xs) = Prob [sumProbs $ allFalse xs, sumProbs $ allTrue xs]
  where
    allTrue xs = filter fst xs
    allFalse xs = filter (not . fst) xs
    sumProbs xs =
      foldl
        (\(val1, prob1) (val2, prob2) -> (val2, prob1 + prob2))
        (False, 0 % 1)
        xs

testProbMonadFlipCoins = getProb $ sumProbs' flipCoins
