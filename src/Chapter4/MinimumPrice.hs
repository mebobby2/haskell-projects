module Chapter4.MinimumPrice where

import Data.Monoid (Monoid, (<>), mempty, mappend)

data TravelGuide = TravelGuide { title :: String, authors :: [String], price :: Double }
                 deriving (Show, Eq, Ord)


treeInsert :: TravelGuide -> BinaryTree -> BinaryTree
treeInsert t n@(Node v l r) = case compare t v of
                                EQ -> n
                                LT -> Node v (treeInsert t l) r
                                GT -> Node v l (treeInsert t r)
treeInsert t Leaf           = Node t Leaf Leaf

--1. Simple Binary Trees

data BinaryTree = Node TravelGuide BinaryTree BinaryTree
                | Leaf
                deriving Show

treeFind :: TravelGuide -> BinaryTree -> Maybe TravelGuide
treeFind t (Node v l r) = case compare t v of
                            EQ -> Just v
                            LT -> treeFind t l
                            GT -> treeFind t r
treeFind _ Leaf         = Nothing

--2. Polymorphic Binary Trees

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a)
                   | Leaf2
                   deriving Show

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) = case compare t v of
                             EQ -> Just v
                             LT -> treeFind2 t l
                             GT -> treeFind2 t r
treeFind2 _ Leaf2         = Nothing

newtype TravelGuidePrice = TravelGuidePrice TravelGuide deriving Eq
instance Ord TravelGuidePrice where
  (TravelGuidePrice (TravelGuide t1 a1 p1)) <= (TravelGuidePrice (TravelGuide t2 a2 p2)) =
    p1 < p2 || (p1 == p2) && (t1 < t2 || (t1 == t2 && a1 <= a2))

--3. Binary Trees with Monoidal Cache

data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
                     | Leaf3
                     deriving (Show, Eq, Ord)

treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c (Node3 v2 c2 l r) = case compare v v2 of
                                      EQ -> Node3 v2 c2 l r
                                      LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r
                                      GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)
treeInsert3 v c Leaf3             = Node3 v c Leaf3 Leaf3

treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c (Node3 v2 c2 l r) = case compare v v2 of
                                     EQ -> Node3 v2 c2 l r
                                     LT -> let newLeft = treeInsert4 v c l
                                               newCache = c2 <> cached newLeft <> cached r
                                            in Node3 v2 newCache newLeft r
                                     GT -> let newRight = treeInsert4 v c r
                                               newCache = c2 <> cached l <> cached newRight
                                            in Node3 v2 newCache l newRight
treeInsert4 v c Leaf3            = Node3 v c Leaf3 Leaf3

cached :: Monoid c => BinaryTree3 v c -> c
cached (Node3 _ c _ _) = c
cached Leaf3           = mempty

newtype Min = Min Double deriving Show

instance Monoid Min where
  mempty = Min infinity where infinity = 1/0
  mappend (Min x) (Min y) = Min $ min x y

--tree = Node3 7 (Min 7) Leaf3 Leaf3
--tree2 = treeInsert4 4 (Min 4) tree
--tree3 = treeInsert4 11 (Min 11) tree2
--tree4 = treeInsert4 2 (Min 2) tree3
--tree5 = treeInsert4 5 (Min 5) tree4
--tree6 = treeInsert4 8 (Min 8) tree5
--tree7 = treeInsert4 2 (Min 2) tree6

--Notice how the cached value at each node is the minimum price


--If you want to built it with TravelGuides
--Node3 (TravelGuide "A" ["A"] 7.0) (Min 7.0) Leaf3 Leaf3


