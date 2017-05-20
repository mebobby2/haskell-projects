module Chapter4.MinimumPrice where

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

