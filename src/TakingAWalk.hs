-- src: http://learnyouahaskell.com/zippers
data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

freeTree :: Tree Char
freeTree =
  Node
    'P'
    (Node
       'O'
       (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty))
       (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty)))
    (Node
       'L'
       (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty))
       (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))

-- comment this out to see the tree format (do because of auto formatting)
-- freeTree :: Tree Char
-- freeTree =
--     Node 'P'
--         (Node 'O'
--             (Node 'L'
--                 (Node 'N' Empty Empty)
--                 (Node 'T' Empty Empty)
--             )
--             (Node 'Y'
--                 (Node 'S' Empty Empty)
--                 (Node 'A' Empty Empty)
--             )
--         )
--         (Node 'L'
--             (Node 'W'
--                 (Node 'C' Empty Empty)
--                 (Node 'R' Empty Empty)
--             )
--             (Node 'A'
--                 (Node 'A' Empty Empty)
--                 (Node 'C' Empty Empty)
--             )
--         )
-- TASK: chang the W in the tree to a P.
-- we could store the path to the element and recursively walk that path.
data Direction
  = L
  | R
  deriving (Show)

type Directions = [Direction]

-- changes elem at position based on directions.
changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

printTree = freeTree

printChangedTree = changeToP [R, L] freeTree

-- prints element at position based on directions.
elemAt :: Directions -> Tree Char -> Char
elemAt [] (Node x _ _) = x
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r

printChangedElem = elemAt [R, L] $ changeToP [R, L] freeTree

-- using this we have to walk the whole path to find an elem for every elem we
-- want to change..
--
-- leave breadcrumbs whenever we move :D
type Breadcrumbs = [Direction]

-- walkTree :: Directions -> Tree a -> Breadcrumbs -> (Tree a, Breadcrumbs)
-- walkTree [] (Node x _ _) = (x, [])
-- walkTree (L:ds) (Node x l r) bs = ((Node x (walkTree ds l) r), bs ++ [L])
-- walkTree (R:ds) (Node _ _ R) = ((Node x l (walkTree ds r), bs ++ [R])
goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft ((Node _ l _), bs) = (l, L : bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight ((Node _ _ r), bs) = (r, R : bs)

walkTree1 = goLeft $ goRight (freeTree, [])

-- use x -: f = f x   --> to enable putting value first and then the functions.
x -: f = f x

-- if we want to go back up from our breadcrumbs, we have to store the
-- information of the previous parent and the subtree we did not take.
walkTree2 = (freeTree, []) -: goRight -: goLeft

data Crumb a
  = LeftCrumb a (Tree a)
  | RightCrumb a (Tree a)
  deriving (Show, Eq)

type Crumbs a = [Crumb a]

goLeft1 :: (Tree a, Crumbs a) -> (Tree a, Crumbs a)
goLeft1 (Node x l r, bs) = (l, LeftCrumb x r : bs)

goRight1 :: (Tree a, Crumbs a) -> (Tree a, Crumbs a)
goRight1 (Node x l r, bs) = (r, RightCrumb x l : bs)

goUp :: (Tree a, Crumbs a) -> (Tree a, Crumbs a)
goUp (tree, (LeftCrumb x rightSubTree:bs)) = (Node x tree rightSubTree, bs)
goUp (tree, (RightCrumb x leftSubTree:bs)) = (Node x leftSubTree tree, bs)

-- walk path to create equal trees.
walkTree3 = (freeTree, []) -: goLeft1 -: goRight1

walkTree4 = (freeTree, []) -: goLeft1 -: goRight1 -: goUp -: goRight1

walkTree5 =
  (freeTree, []) -: goLeft1 -: goRight1 -: goUp -: goRight1 -: goLeft1 -: goUp

isEqualTree = all (== walkTree3) [walkTree4, walkTree5]

-- --> data structure which allow for focused part of data (easy move up and
-- down in tree) is called a zipper in haskell.
type Zipper a = (Tree a, Crumbs a)

-- alternative naming.
-- type Focus a = (Tree a, Crumbs a)
-- manipulate current root of the focused/zipped node.
changeRoot :: (a -> a) -> Zipper a -> Zipper a
changeRoot f (Node x l r, bs) = (Node (f x) l r, bs)

testChangeRoot1 = walkTree5

testChangeRoot2 = changeRoot (\_ -> 'Q') walkTree5

walkTree6 =
  (freeTree, []) -: goLeft1 -: goRight1 -: goUp -: goRight1 -:
  changeRoot (\_ -> 'Q')

-- replaces current node with given subtree.
attach :: Tree a -> Zipper a -> Zipper a
attach tree (_, bs) = (tree, bs)

farLeft = (freeTree, []) -: goLeft1 -: goLeft1 -: goLeft1 -: goLeft1

changeFocus = farLeft -: attach (Node 'Q' Empty Empty)

moveToRoot :: Zipper a -> Zipper a
moveToRoot (tree, []) = (tree, [])
moveToRoot zipper = moveToRoot $ goUp zipper

-- Now we are able to walk around the tree and modify and attach whatever,
-- whereever we want. If we are finished we can just get the resulting tree by
-- going up once using moveToRoot.
-- Zippers/Focuses can be used on any data structure. Yeeees, also on lists :P
-- FOCUSING ON LISTS
-- comparing this to our tree definition we can see how a list can basicly be
-- seen as a tree with only one leaf node.
type ListZipper a = ([a], [a])

goNext :: ListZipper a -> ListZipper a
goNext (x:xs, ys) = (xs, x : ys)

goPrevious :: ListZipper a -> ListZipper a
goPrevious (xs, y:ys) = (y : xs, ys)

freeList = ([1, 2, 3, 4, 5, 100, 101, 102], [])

testListZipper1 = freeList -: goNext -: goNext

testListZipper2 = freeList -: goNext -: goNext -: goNext

testListZipper3 = freeList -: goNext -: goNext -: goNext -: goPrevious

testEqualZipper = testListZipper1 == testListZipper3

-- "If you were making a text editor, you could use a list of strings to
-- represent the lines of text that are currently opened and you could then use
-- a zipper so that you know which line the cursor is currently focused on. By
-- using a zipper, it would also make it easier to insert new lines anywhere in
-- the text or delete existing ones. "
--
--
-- A VERY SIMPLE FILE SYSTEM
type Name = String

type Data = String

data FSItem
  = File Name Data
  | Folder Name [FSItem]
  deriving (Show, Eq)

freeFolder = Folder "src" [File "hello.hs" "data", File "world.hs" "data"]

-- sample file system
myDisk :: FSItem
myDisk =
  Folder
    "root"
    [ File "goat_yelling_like_man.wmv" "baaaaaa"
    , File "pope_time.avi" "god bless"
    , Folder
        "pics"
        [ File "ape_throwing_up.jpg" "bleargh"
        , File "watermelon_smash.gif" "smash!!"
        , File "skull_man(scary).bmp" "Yikes!"
        ]
    , File "dijon_poupon.doc" "best mustard"
    , Folder
        "programs"
        [ File "fartwizard.exe" "10gotofart"
        , File "owl_bandit.dmg" "mov eax, h00t"
        , File "not_a_virus.exe" "really not a virus"
        , Folder
            "source code"
            [ File "best_hs_prog.hs" "main = print (fix error)"
            , File "random.hs" "main = print 4"
            ]
        ]
    ]

-- breadcrumbs will be folder. Crumb will be a folder containing all filesystem
-- items for the current level (besides the current file).
type FSCrumbs = [FSItem]

type FSZipper = (FSItem, FSCrumbs)

-- filesystem items one level down
fsDown :: FSZipper -> FSZipper
fsDown (Folder x (y:ys), []) = (y, [Folder x ys])

-- filesystem items one level up
fsUp :: FSZipper -> FSZipper
fsUp (_, []) = undefined
fsUp (fsItem, (Folder name items:folders)) =
  (Folder name (fsItem : items), folders)

-- next filesystem item in same level
fsNext :: FSZipper -> FSZipper
fsNext (_, []) = undefined
fsNext (fsItem, (Folder name (item:items):folders)) =
  (item, Folder name (items ++ [fsItem]) : folders)

-- previous filesystem item in same level
fsPrev :: FSZipper -> FSZipper
fsPrev (_, []) = undefined
fsPrev (fsItem, (Folder name (item:items):folders)) =
  (last items, Folder name (fsItem : init items) : folders)

-- SOLUTION: (above code works, but is/was somewhat hacky for next and prev)
-- Here a crumb consists of the parent folder, and the files inside the folder
-- that come before and after the current file.
data FSCrumb1 =
  FSCrumb1 Name [FSItem] [FSItem]
  deriving (Show, Eq)

type FSZipper1 = (FSItem, [FSCrumb1])

fsUp1 :: FSZipper1 -> FSZipper1
fsUp1 (item, FSCrumb1 name ls rs:restCrumbs) =
  (Folder name (ls ++ [item] ++ rs), restCrumbs)

fsDown1 :: FSZipper1 -> FSZipper1
fsDown1 (Folder name (item:items), crumbs) =
  (item, FSCrumb1 name [] items : crumbs)

-- next goes to right
fsNext1 :: FSZipper1 -> FSZipper1
fsNext1 (item, FSCrumb1 name ls (r:rs):restCrumbs) =
  (r, FSCrumb1 name (item : ls) rs : restCrumbs)

-- prev goes to left
fsPrev1 :: FSZipper1 -> FSZipper1
fsPrev1 (item, FSCrumb1 name (l:ls) rs:restCrumbs) =
  (l, FSCrumb1 name ls (item : rs) : restCrumbs)

testFS0 = (myDisk, [])

testFS1 = fsDown1 $ (myDisk, [])

testFS2 = (myDisk, []) -: fsDown1

testFS3 = (myDisk, []) -: fsDown1 -: fsNext1

testFS4 = (myDisk, []) -: fsDown1 -: fsNext1 -: fsNext1

testFS5 = (myDisk, []) -: fsDown1 -: fsNext1 -: fsNext1 -: fsNext1 -: fsPrev1

testFS6 = (myDisk, []) -: fsDown1 -: fsNext1 -: fsNext1 -: fsDown1 -: fsUp1

testEqualFS = all (== testFS4) [testFS5, testFS6]

-- focus a file by name in given folder
selectFSItem :: Name -> FSZipper1 -> FSZipper1
selectFSItem name (Folder folderName items, restCrumbs) =
  let (ls, item:rs) = break (nameIs name) items
   in (item, FSCrumb1 folderName ls rs : restCrumbs)

-- checks wheter name matches with folder or file name
nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

testSelectFS1 = testFS0 -: selectFSItem "pope_time.avi"

testSelectFS2 =
  (myDisk, []) -: selectFSItem "pics" -: selectFSItem "skull_man(scary).bmp"

-- renaming a file.
fsRename1 :: Name -> FSZipper1 -> FSZipper1
fsRename1 newName (File name content, crumbs) = (File newName content, crumbs)
fsRename1 newName (Folder name content, crumbs) =
  (Folder newName content, crumbs)

testFsRename1 = (myDisk, []) -: selectFSItem "pics" -: fsRename1 "cspi" -: fsUp1

-- adding new item into current folder
fsAddItem :: FSItem -> FSZipper1 -> FSZipper1
fsAddItem newItem (Folder name items, crumbs) =
  (Folder name $ items ++ [newItem], crumbs)

testFsAddItem =
  (myDisk, []) -: fsAddItem (File "testfile!" "woaaah, test content!")

-- NOTE: skips doing Maybe Monad for the Tree. Instead do the Maybe Monad for
-- the file system.
-- currently our functions just straight up trow an error if we run into issues
-- (going up with no crumbs, going left when subtree is empty, ...)
-- --> get decent error handling by using our Maybe Monad! :]
fsUp2 :: FSZipper1 -> Maybe FSZipper1
fsUp2 (Folder currentName content, FSCrumb1 name ls rs:restCrumbs) =
  Just (Folder name (ls ++ [(Folder currentName content)] ++ rs), restCrumbs)
fsUp2 (File _ _, _) = Nothing

fsDown2 :: FSZipper1 -> Maybe FSZipper1
fsDown2 (Folder name (item:items), crumbs) =
  Just (item, FSCrumb1 name [] items : crumbs)
fsDown2 (File _ _, _) = Nothing

-- prev goes to right
fsNext2 :: FSZipper1 -> Maybe FSZipper1
fsNext2 (_, []) = Nothing
fsNext2 (item, FSCrumb1 name ls (r:rs):restCrumbs) =
  Just (r, FSCrumb1 name (item : ls) rs : restCrumbs)

-- prev goes to left
fsPrev2 :: FSZipper1 -> Maybe FSZipper1
fsPrev2 (_, []) = Nothing
fsPrev2 (item, FSCrumb1 name (l:ls) rs:restCrumbs) =
  Just (l, FSCrumb1 name ls (item : rs) : restCrumbs)

-- testFS2 = (myDisk, []) -: fsDown1
--
-- testFS3 = (myDisk, []) -: fsDown1 -: fsNext1
--
-- testFS4 = (myDisk, []) -: fsDown1 -: fsNext1 -: fsNext1
--
-- testFS5 = (myDisk, []) -: fsDown1 -: fsNext1 -: fsNext1 -: fsNext1 -: fsPrev1
--
-- testFS6 = (myDisk, []) -: fsDown1 -: fsNext1 -: fsNext1 -: fsDown1 -: fsUp1
testMaybeFs1 = (myDisk, [])

testMaybeFs2 = return (myDisk, []) >>= fsDown2 >>= fsNext2

testMaybeFs3 = return (myDisk, []) >>= fsDown2 >>= fsNext2 >>= fsNext2

-- check impossible case (without Maybe Monad, does return error)
testFsError4 =
  (myDisk, []) -: fsDown1 -: fsDown1 -: fsDown1 -: fsDown1 -: fsDown1

-- with Maybe Monad, graceful Nothing. With Either Monad we could add a specific
-- error message as well:]
testMaybeFs4 =
  return (myDisk, []) >>= fsDown2 >>= fsDown2 >>= fsDown2 >>= fsDown2 >>=
  fsDown2
-- WE ARE DONE WITH THE LEARN YOU A HASKELL BOOK ! :]
