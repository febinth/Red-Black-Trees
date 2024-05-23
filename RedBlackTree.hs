module RedBlack where

-- Definition of the Red-Black Tree data structure

data Color = Red | Black
  deriving (Eq,Show)

data RBT a = LeafRB | NodeRB Color (RBT a) a (RBT a)
  deriving (Eq,Show)


-- Serching a key inside a red-black tree
--   return True if the key is found, False otherwise

searchRB :: Ord a => a -> RBT a -> Bool


-- Minimum and maximum of red-black tree
--   return Nothing if the tree is empty

minRB :: RBT a -> Maybe a

maxRB :: RBT a -> Maybe a
  

-- Check if a tree satisfies the Binary Search Tree condition
--   (do not check other RBT conditions)
isBST :: Ord a => RBT a -> Bool


-- Check the Black-balancing condition:
--     all paths have the same number of black nodes

blackBalanced :: RBT a -> Bool


-- Black height of a black-balanced tree, -1 if not black-balanced

blackHeight :: RBT a -> Int


-- Check if all Red-Black Tree conditions are satisfied
isRBT :: Ord a => RBT a -> Bool


-- Insert a new element in a RBT, preserving the RBT properties

insertRB :: Ord a => a -> RBT a -> RBT a


-- Delete an element from a RBT, preserving the RBT properties

deleteRB :: Ord a => a -> RBT a -> RBT a


