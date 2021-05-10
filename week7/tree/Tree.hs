{-# OPTIONS_GHC -Wall #-}

import Prelude hiding ( filter )


import Data.List ( sort )
import Test.QuickCheck

--
-- Write the "worst" and "best" versions of filter you can think of
--

{-
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f xs = if null xs then [] else (if f (head xs) then (head xs) : filter f (tail xs) else filter f (tail xs))

filter _ []     = []
filter f (x:xs)
    | f x       = x : filter f xs
    | otherwise = filter f xs
-}

--
-- Define a data type for binary trees
--

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

--
-- Define a function that computes the height of a tree.
--

height :: Tree a -> Integer
height Leaf         = 1
height (Node l _ r) = 1 + max (height l) (height r)

--
-- Write a predicate that is true iff a tree is balanced. A tree is balanced
-- iff:
--
-- 1. The heights of the left and right subtrees differ by at most 1
-- 2. The left and right subtrees are balanced.
--

isBalanced :: Tree a -> Bool
isBalanced Leaf         = True
isBalanced (Node l _ r) = isBalanced l && isBalanced r && abs (height l - height r) <= 1

--
-- Returns elements of tree encountered during inorder traversal
--

inorder :: Tree a -> [a]
inorder Leaf         = []
inorder (Node l x r) = inorder l ++ x : inorder r

--
-- Write a predicate to determine if a tree is a balanced search tree
--

isBalancedSearchTree :: Ord a => Tree a -> Bool
isBalancedSearchTree t = isBalanced t && sorted (inorder t)

sorted :: Ord a => [a] -> Bool
sorted xs = sort xs == xs

--
-- Write a function to build a balanced binary search tree from the elements in
-- the list xs. Do not assume xs is sorted.
--

buildBalancedSearchTree :: Ord a => [a] -> Tree a
buildBalancedSearchTree xs0 = build (xs0)
  where
    build :: [a] -> Tree a
    build [] = Leaf
    build xs = Node (build ys) z (build zs)
      where
        n    = length xs
        ys   = take (n `div` 2) xs
        z:zs = drop (n `div` 2) xs

--
-- Write a property that asserts a BST we build is really a BST.
--

prop_built_tree_is_bst :: [Int] -> Bool
prop_built_tree_is_bst xs = isBalancedSearchTree (buildBalancedSearchTree xs)
