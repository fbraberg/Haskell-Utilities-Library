module BinarySearchTree where

import Data.Char
import Data.List
import Maths



data BST a = BST (Node a) deriving (Show)

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

nodes = map (\(k,v)->createNode k v Nothing Nothing) $ zip  [1..10] ['a'..'j']

createBST :: Ord a => [Node a] -> BST a
createBST nodes = undefined


createNode :: Int -> a -> Maybe (Node a) -> Maybe (Node a)  -> Node a
createNode key val left right = Node key val left right
