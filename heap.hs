-- {-# LANGUAGE DatatypeContexts #-}
-- data (Ord a) => Tree a = Null | Fork a (Tree a) (Tree a)
-- 上のghc拡張入れないとコンパイルできない
-- データ型に型制約を入れる拡張
-- ちなみにdeprecated
-- https://downloads.haskell.org/~ghc/7.8.3/docs/html/users_guide/data-type-extensions.html

{-  Sec.1.1
data Tree a = Null | Fork a (Tree a) (Tree a)
  deriving (Eq, Ord, Show)

isEmpty :: Tree a -> Bool
isEmpty Null = True
isEmpty (Fork x a b) = False

minElem :: Tree a -> a
minElem (Fork x a b) = x

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Fork x a b) = merge a b

insert :: Ord a => a -> Tree a -> Tree a
insert x a = merge (Fork x Null Null) a

merge :: Ord a => Tree a -> Tree a -> Tree a
merge a Null = a
merge Null b = b
merge a b
  | minElem a <= minElem b = join a b
  | otherwise              = join b a

join :: Ord a => Tree a -> Tree a -> Tree a
join (Fork x a b) c = Fork x (merge a b) c
-}

{---  Sec.1.2
import Data.Tree hiding (Tree)
toDataTree Null = Node Nothing []
toDataTree (Fork n x a b) = Node (Just x) [toDataTree a, toDataTree b]

printTree t = putStr $ drawTree $ fmap show $ toDataTree t


data Tree a = Null | Fork Int a (Tree a) (Tree a)
  deriving (Show)

isEmpty :: Tree a -> Bool
isEmpty Null = True
isEmpty (Fork n x a b) = False

minElem :: Tree a -> a
minElem (Fork n x a b) = x

deleteMin :: (Ord a) => Tree a -> Tree a
deleteMin (Fork n x a b) = merge a b

insert :: (Ord a) => a -> Tree a -> Tree a
insert x a = merge (Fork 1 x Null Null) a

merge :: (Ord a) => Tree a -> Tree a -> Tree a
merge a Null = a
merge Null b = b
merge a b
  | minElem a <= minElem b = join a b
  | otherwise              = join b a

join :: (Ord a) => Tree a -> Tree a -> Tree a
join (Fork n x a b) c = Fork (n + size c) x aa (merge bb cc)
  where (aa, bb, cc) = orderBySize a b c

orderBySize :: Tree a -> Tree a -> Tree a -> (Tree a, Tree a, Tree a)
orderBySize a b c
  | size a == biggest = (a, b, c)
  | size b == biggest = (b, a, c)
  | size c == biggest = (c, a, b)
  where
    biggest = size a `max` size b `max` size c

size :: Tree a -> Int
size Null = 0
size (Fork n x a b) = n
--}

--{-  Sec.1.4
--{- RoundRobin heap
import Data.Tree hiding (Tree)
import qualified Data.List as L
toDataTree Null = Node Nothing []
toDataTree (Fork n x a b) = Node (Just (n,x)) [toDataTree a, toDataTree b]

printTree t = putStr $ drawTree $ fmap show $ toDataTree t


data Color = Blue | Red
  deriving (Show, Eq)
data Tree a = Null | Fork Color a (Tree a) (Tree a)
  deriving (Show, Eq)

isEmpty :: Tree a -> Bool
isEmpty Null = True
isEmpty (Fork c x a b) = False

minElem :: Tree a -> a
minElem (Fork c x a b) = x

deleteMin :: (Ord a) => Tree a -> Tree a
deleteMin (Fork c x a b) = merge a b

insert :: (Ord a) => a -> Tree a -> Tree a
insert x a = merge (Fork Blue x Null Null) a

merge :: (Ord a) => Tree a -> Tree a -> Tree a
merge a Null = a
merge Null b = b
merge a b
  | minElem a <= minElem b = join a b
  | otherwise              = join b a

join :: (Ord a) => Tree a -> Tree a -> Tree a
join (Fork Blue x a b) c = Fork Red x (merge a c) b
join (Fork Red x a b) c = Fork Blue x a (merge b c)

--}

{- self-adjusting heap
import Data.Tree hiding (Tree)
import qualified Data.List as L
toDataTree Null = Node Nothing []
toDataTree (Fork x a b) = Node (Just x) [toDataTree a, toDataTree b]

printTree t = putStr $ drawTree $ fmap show $ toDataTree t


data Tree a = Null | Fork a (Tree a) (Tree a)
  deriving (Eq, Show)
isEmpty :: Tree a -> Bool
isEmpty Null = True
isEmpty (Fork x a b) = False

minElem :: (Ord a) => Tree a -> a
minElem (Fork x a b) = x

deleteMin :: (Ord a) => Tree a -> Tree a
deleteMin (Fork x a b) = merge a b

insert :: (Ord a) => a -> Tree a -> Tree a
insert x a = merge (Fork x Null Null) a

merge :: (Ord a) => Tree a -> Tree a -> Tree a
merge a Null = a
merge Null b = b
merge a b
  | minElem a <= minElem b = join a b
  | otherwise              = join b a

join :: (Ord a) => Tree a -> Tree a -> Tree a
join (Fork x a b) c = Fork x b (merge a c)

-}
-- Ex.1.5
search :: (Eq a, Ord a) => Tree a -> [a] -> [[a]]
search t l = [x | x<-L.permutations l, t==insertElems x]
  where insertElems l = foldl (flip insert) Null l

-- printTree $ foldl (flip insert) Null [7,9,8,5,6,3,4,1,2]
-- minElemより小さい値をinsertすると、Redでrootに挿入できる
--- search (Fork Blue 1 (Fork Blue 3 (Fork Blue 5 (Fork Blue 7 (Fork Blue 9 Null Null) (Fork Blue 8 Null Null)) (Fork Blue 6 Null Null)) (Fork Blue 4 Null Null)) (Fork Blue 2 Null Null)) [1..9]
--- search (Fork Blue 1 (Fork Blue 2 Null Null) (Fork Blue 3 (Fork Blue 4 Null Null) (Fork Blue 5 (Fork Blue 6 Null Null) (Fork Blue 7 (Fork Blue 8 Null Null) (Foprk Blue 9 Null Null))))) [1..9]


-- search (Fork 1 (Fork 3 (Fork 5 (Fork 7 (Fork 9 Null Null) (Fork 8 Null Null)) (Fork 6 Null Null)) (Fork 4 Null Null)) (Fork 2 Null Null)) [1..9]
-- search (Fork 1 (Fork 2 Null Null) (Fork 3 (Fork 4 Null Null) (Fork 5 (Fork 6 Null Null) (Fork 7 (Fork 8 Null Null) (Fork 9 Null Null))))) [1..9]

---}
