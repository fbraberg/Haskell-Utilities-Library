module Datastructures.BinarySearchTree where

import Data.Char
import Data.List


type Root a = Node a
data BST a = BST (Root a) deriving (Show)

-- ==========================/Node\==========================
data Node a = Node {keyVal :: Int
                   ,val :: a
                   ,left :: Maybe (Node a)
                   ,right :: Maybe (Node a)} deriving (Eq)

instance Show a => Show (Node a) where
    show (Node key val _ _) =
        ("Key: " ++ (show key) ++ " Value: " ++ show val ++ "\n")

instance Eq a => Ord (Node a) where
    (<=) (Node key1 val1 _ _) (Node key2 val2 _ _) = key1 <= key2
    (>=) (Node key1 val1 _ _) (Node key2 val2 _ _) = key1 >= key2
-- ==========================\Node/==========================

createBST :: Ord a => [Node a] -> [Node a]
createBST nodes = undefined -- Work In Progress


createNode :: Int -> a -> Maybe (Node a) -> Maybe (Node a)  -> Node a
createNode key val left right = Node key val left right
