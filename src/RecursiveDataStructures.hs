-- define own list with ADT (NOTE: ADT="Algebraic Data Types" -> haskell name
-- for type definition)
-- data list' a :: a list'
-- either empty list or some value with a list of values of same type.
data List' a = TestEmpty | Cons a (List' a) deriving (Show, Eq, Ord, Read)
-- cons is ':' in normal lists.
-- calling cons in infix to create a list'
testList' = 3 `Cons` TestEmpty
testLargeList' = 3 `Cons` (4 `Cons` (5 `Cons` TestEmpty))

-- "We can define functions to be automatically infix by making them comprised
-- of only special characters. We can also do the same with constructors, since
-- they're just functions that return a data type. So check this out."
infixr 5 :.:
data List a = Empty | a :.: (List a) deriving (Show, Read, Eq, Ord)
testLista = 2 :.: 3 :.: 4 :.: Empty
-- infixr val symbol -> means that symbol is right associative with value val.
-- infixl val symbol -> means that symbol is left associative with value val.
-- example: + is defined with fixity 6, infixr6.
-- * is defined with fixity 7. inifxr7.
-- Meaning that both are right associative -> a * b * c === a * (b * c)
-- Since * has higher right fixity -> a + b * c === a + (b * c)
-- BUT a * b + c !== a * (b + c)


-- concat two lists is defined as:
-- infixr 5  ++
-- (++) :: [a] -> [a] -> [a]
-- []     ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)
-- custom concat two lists. uses our previous definitions :] (Empty, List, :.:,
-- etc.)
infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :.: xs) .++ ys = x :.: (xs .++ ys)
-- TODO(pierre): understand this text..
-- NOTE: xs is List a x is x and :.: is :.. -> (x :.: xs) === a :.. (List a)
-- "Notice how we pattern matched on (x :-: xs). That works because pattern
-- matching is actually about matching constructors. We can match on :-: because
-- it is a constructor for our own list type and we can also match on : because
-- it is a constructor for the built-in list type. Same goes for []. Because
-- pattern matching works (only) on constructors, we can match for stuff like
-- that, normal prefix constructors or stuff like 8 or 'a', which are basically
-- constructors for the numeric and character types, respectively."


-- BINARY SEARCH TREE (BST)
-- LEFT FROM = -> type constructor -> cant be created as value
-- RIGHT FROM = -> value constructor -> can be created as value
-- -> There will never be BST 1 as value. Since BST 1 is either EmptyTree or
-- Node 1 (EmptyTree | Node val (EmptyTree | Node val ....))
data BST a = EmptyTree | Node a (BST a) (BST a) deriving (Show, Read, Eq)
emptyTree a = Node a EmptyTree EmptyTree

-- TODO: continue at binary search tree.
