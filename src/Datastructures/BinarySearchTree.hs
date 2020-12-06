module BinarySearchTree where

import Data.Char
import Data.List
import Maths



data BST a = BST (Node a) deriving (Show)

-- ==========================/Node\==========================
data Node a = Node {keyVal :: Int
                   ,val :: a
                   ,left :: Node a
                   ,right :: Node a} | End deriving (Eq)

instance Show a => Show (Node a) where
    show (Node key val left right) =
        ("Key: " ++ (show key) ++ ", Value: " ++ show val ++
         ", Left node: " ++ (show left) ++ ", Right node: " ++ (show right) ++ "\n")
    show End = "NULL"


instance Eq a => Ord (Node a) where
    (<=) (Node key1 val1 _ _) (Node key2 val2 _ _) = key1 <= key2
    (>=) (Node key1 val1 _ _) (Node key2 val2 _ _) = key1 >= key2
-- ==========================\Node/==========================

lNodes = map (\(k,v)->createNode k v End End) $ zip  [1..]
                        [[c1] ++ [c2] | c1 <- ['a'..'z'], c2 <- ['a'..'z']]
mNodes = map (\(k,v)->createNode k v End End) $ zip  [1..] ['a'..'z']
sNodes = map (\(k,v)->createNode k v End End) $ zip  [1..] ['a'..'c']

alphabetTree = createBST mNodes

createBST :: Ord a => [Node a] -> BST a
createBST nodes = BST $ createBST_helper nodes

createBST_helper :: Ord a => [Node a] -> Node a
createBST_helper [Node k v End End] = Node k v End End
createBST_helper nodes = Node k v leftBranch rightBranch
    where
        midIndex = div (length nodes) 2
        Node k v _ _ = head $ drop midIndex nodes
        leftBranch = createBST_helper $ take midIndex nodes
        rightBranch = createBST_helper $ drop (midIndex+1) nodes


searchBST :: Node a -> BST a -> Node a
searchBST (Node k v End End) (BST (Node key val left right))
    | k < key   = searchBST (Node k v End End) (BST left)
    | k > key   = searchBST (Node k v End End) (BST right)
    | otherwise = Node key val left right

createNode :: Int -> a -> Node a -> Node a  -> Node a
createNode key val left right = Node key val left right
