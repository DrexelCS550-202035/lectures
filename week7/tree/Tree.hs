{-# OPTIONS_GHC -Wall #-}

import Data.List (sort)
import Test.QuickCheck

--
-- Write the "worst" and "best" versions of filter you can think of
--

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

--
-- Define a data type for binary trees
--

data Tree a = Tree

--
-- Define a function that computes the height of a tree.
--

height :: Tree a -> Integer
height = undefined

--
-- Write a predicate that is true iff a tree is balanced. A tree is balanced
-- iff:
--
-- 1. The heights of the left and right subtrees differ by at most 1
-- 2. The left and right subtrees are balanced.
--

isBalanced :: Tree a -> Bool
isBalanced = undefined

--
-- Returns elements of tree encountered during inorder traversal
--

inorder :: Tree a -> [a]
inorder = undefined

--
-- Write a predicate to determine if a tree is a balanced search tree
--

isBalancedSearchTree :: Ord a => Tree a -> Bool
isBalancedSearchTree = undefined

--
-- Write a function to build a balanced binary search tree from the elements in
-- the list xs. Do not assume xs is sorted.
--

buildBalancedSearchTree :: Ord a => [a] -> Tree a
buildBalancedSearchTree = undefined

--
-- Write a property that asserts a BST we build is really a BST.
--

prop_built_tree_is_bst :: [Int] -> Bool
prop_built_tree_is_bst = undefined
