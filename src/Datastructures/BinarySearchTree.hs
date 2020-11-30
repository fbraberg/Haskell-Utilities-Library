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

mNodes = map (\(k,v)->createNode k v End End) $ zip  [1..10] ['a'..'j']
sNodes = map (\(k,v)->createNode k v End End) $ zip  [1..3] ['a'..'c']

createBST :: Ord a => [Node a] -> Node a
createBST [Node k v End End] = Node k v End End
createBST nodes = Node k v leftBranch rightBranch
    where
        midIndex = div (length nodes) 2
        Node k v _ _ = head $ drop midIndex nodes
        leftBranch = createBST $ take midIndex nodes
        rightBranch = createBST $ drop (midIndex+1) nodes


createNode :: Int -> a -> Node a -> Node a  -> Node a
createNode key val left right = Node key val left right
