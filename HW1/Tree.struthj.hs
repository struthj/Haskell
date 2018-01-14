module Tree where
-- imports 
import Data.List (sort)
-- 
-- * Part 1: Binary trees
-- Author: Struth, Joseph 
--


-- Sources: https://www.geeksforgeeks.org/tree-traversals-inorder-preorder-and-postorder/
-- | Integer-labeled binary trees.
data Tree = Node Int Tree Tree   -- ^ Internal nodes
          | Leaf Int             -- ^ Leaf nodes
  deriving (Eq,Show)


-- | An example binary tree, which will be used in tests.
t1 :: Tree
t1 = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5))
                    (Leaf 6))
            (Node 7 (Leaf 8) (Leaf 9))

-- | Another example binary tree, used in tests.
t2 :: Tree
t2 = Node 6 (Node 2 (Leaf 1) (Node 4 (Leaf 3) (Leaf 5)))
            (Node 8 (Leaf 7) (Leaf 9))


-- | The integer at the left-most node of a binary tree.
--
--   >>> leftmost (Leaf 3)
--   3
--
--   >>> leftmost (Node 5 (Leaf 6) (Leaf 7))
--   6
--   
--   >>> leftmost t1
--   4
--
--   >>> leftmost t2
--   1
--
leftmost :: Tree -> Int
leftmost (Leaf i)     = i
leftmost (Node _ l _) = leftmost l


-- | The integer at the right-most node of a binary tree.
--
--   >>> rightmost (Leaf 3)
--   3
--
--   >>> rightmost (Node 5 (Leaf 6) (Leaf 7))
--   7
--   
--   >>> rightmost t1
--   9
--
--   >>> rightmost t2
--   9
--
rightmost :: Tree -> Int 
rightmost (Leaf r) = r
rightmost (Node _ _ r) = rightmost r
--- Where the first case is a single Leaf with int r
--- Second case is patter match any for the int, left node.
--- Then calling rightmost on the right node until the first case is matched
---
-- | Get the maximum integer from a binary tree.
--
--   >>> maxInt (Leaf 3)
--   3
--
--   >>> maxInt (Node 5 (Leaf 4) (Leaf 2))
--   5
--
--   >>> maxInt (Node 5 (Leaf 7) (Leaf 2))
--   7
--
--   >>> maxInt t1
--   9
--
--   >>> maxInt t2
--   9
--
maxInt:: Tree -> Int
maxInt (Leaf l) = l
maxInt (Node n x y) = maximum [n, (maxInt x), (maxInt y)] 
--- First case is a leaf with int l, where l will be the maximum
--- Second case has int n, and nodes x and y,
--- where using the haskell maximum function, and a list enclosed in  [ ]
--- we will find the max of the int, and recursive calls to left and right trees 
--- ( x and y nodes )

-- | Get the minimum integer from a binary tree.
--
--   >>> minInt (Leaf 3)
--   3
--
--   >>> minInt (Node 2 (Leaf 5) (Leaf 4))
--   2
--
--   >>> minInt (Node 5 (Leaf 4) (Leaf 7))
--   4
--
--   >>> minInt t1
--   1
--
--   >>> minInt t2
--   1
--
minInt:: Tree -> Int
minInt (Leaf l) = l
minInt (Node n x y) = minimum [n, (minInt x), (minInt y)] 
--- First case is a leaf with int l, where l will be the minimum
--- Second case has int n, and nodes x and y,
--- where using the haskell minimum function, and a list enclosed in  [ ]
--- we will find the min of the int, and recursive calls to left and right trees

-- | Get the sum of the integers in a binary tree.
--
--   >>> sumInts (Leaf 3)
--   3
--
--   >>> sumInts (Node 2 (Leaf 5) (Leaf 4))
--   11
--
--   >>> sumInts t1
--   45
--
--   >>> sumInts (Node 10 t1 t2)
--   100
--
sumInts:: Tree -> Int
sumInts (Leaf l) = l
sumInts (Node n x y) = sum[n, (sumInts x), (sumInts y)]
-- First case the single Leaf l will be the sum
-- Second case we will use the haskell sum function to sum
-- the root number, and recursive calls to left and right subtree

-- | The list of integers encountered by a pre-order traversal of the tree.
--
--   >>> preorder (Leaf 3)
--   [3]
--
--   >>> preorder (Node 5 (Leaf 6) (Leaf 7))
--   [5,6,7]
--
--   >>> preorder t1
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> preorder t2
--   [6,2,1,4,3,5,8,7,9]
--   
preorder::Tree -> [Int] 
preorder (Leaf l) = [l]
preorder (Node n x y) = [n] ++ (preorder x) ++ (preorder y)
-- preorder is root, left, right
-- append root, recursive call to left, recursive call to right


-- | The list of integers encountered by an in-order traversal of the tree.
--
--   >>> inorder (Leaf 3)
--   [3]
--
--   >>> inorder (Node 5 (Leaf 6) (Leaf 7))
--   [6,5,7]
--
--   >>> inorder t1
--   [4,3,5,2,6,1,8,7,9]
--
--   >>> inorder t2
--   [1,2,3,4,5,6,7,8,9]
--   
inorder::Tree -> [Int]
inorder (Leaf l) = [l]
inorder (Node n x y) = (inorder x) ++ [n] ++ (inorder y)
-- inorder is left, root, right
-- Using haskell lists
-- recursive call to left subtree, append root, recursive call to right subtree


-- | Check whether a binary tree is a binary search tree.
--
--   >>> isBST (Leaf 3)
--   True
--
--   >>> isBST (Node 5 (Leaf 6) (Leaf 7))
--   False
--   
--   >>> isBST t1
--   False
--
--   >>> isBST t2
--   True
--
isBST:: Tree -> Bool
isBST (Leaf l) = True
isBST (Node n x y) = (inorder (Node n x y)) == sort (inorder (Node n x y))
--- First case if there is just a Leaf it is a valid BST
--- Second case compare the inorder traversal of the Tree, to a sorted list of tree elements
--- As a valid BST will always have a sorted inorder traversal

-- | Check whether a number is contained in a binary search tree.
--   (You may assume that the given tree is a binary search tree.)
--
--   >>> inBST 2 (Node 5 (Leaf 2) (Leaf 7))
--   True
--
--   >>> inBST 3 (Node 5 (Leaf 2) (Leaf 7))
--   False
--
--   >>> inBST 4 t2
--   True
--
--   >>> inBST 10 t2
--   False
--   
inBST:: Int -> Tree -> Bool
inBST num (Node n x y) = num `elem` (inorder (Node n x y))
--- Use haskell elem function to see if integer is present in list
--- list created with inorder function