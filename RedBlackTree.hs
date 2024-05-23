module RedBlackTree where

-- Definition of the Red-Black Tree data structure

data Color = Red | Black
  deriving (Eq,Show)

data RBT a = LeafRB | NodeRB Color (RBT a) a (RBT a)
  deriving (Eq,Show)


-- Serching a key inside a red-black tree
--   return True if the key is found, False otherwise
searchRB :: Ord a => a -> RBT a -> Bool
searchRB key LeafRB = False
searchRB key (NodeRB _ leftRBT a rightRBT)
  | key < a = searchRB key leftRBT -- If key is less than root, search left sub tree
  | key > a = searchRB key rightRBT -- If key is greater than root, search right sub tree
  | otherwise = True -- If key is equal to root, return True


-- Minimum of red-black tree
-- return Nothing if the tree is empty
minRB :: RBT a -> Maybe a
minRB LeafRB = Nothing
minRB (NodeRB _ LeafRB a _) = Just a -- If left sub tree is a leaf return the root
minRB (NodeRB _ leftRBT _ _) = minRB leftRBT -- Recursively check left sub tree for minimum value

-- Maximum of red-black tree
-- return Nothing if the tree is empty
maxRB :: RBT a -> Maybe a
maxRB LeafRB = Nothing
maxRB (NodeRB _ _ a LeafRB) = Just a -- If right sub tree is a leaf return the root
maxRB (NodeRB _ _ _ rightRBT) = maxRB rightRBT -- Recursively check right sub tree for maximum value
  

-- Check if a tree satisfies the Binary Search Tree condition
--   (do not check other RBT conditions)
isBST :: Ord a => RBT a -> Bool
isBST LeafRB = True
isBST (NodeRB _ leftRBT a rightRBT) =
  let leftMax = maxRB leftRBT
      rightMin = minRB rightRBT
      -- leftMax and rightMin might return Nothing since we will compare leaf nodes
      leftTreeCheck = maybe True (a >) leftMax -- If leftMax is Nothing, return True else evaluate a > leftMax
      rightTreeCheck = maybe True (a <) rightMin -- If rightMin is Nothing, return True else evaluate a < rightMin
  in leftTreeCheck && rightTreeCheck && isBST leftRBT && isBST rightRBT


-- Check the Black-balancing condition:
--     all paths have the same number of black nodes
blackBalanced :: RBT a -> Bool
blackBalanced a 
  | null counts = True  -- Handle the empty tree case
  | otherwise = all (== head counts) (tail counts)  -- Check if all elements are equal to the first one
  where counts = countBlackNodes a

countBlackNodes :: RBT a -> [Int]
countBlackNodes LeafRB = [0]
countBlackNodes (NodeRB color leftRBT _ rightRBT) = 
  -- Traverse all paths of the RBT and maintain the count of black nodes 
  let blackCountLeft = countBlackNodes leftRBT
      blackCountRight = countBlackNodes rightRBT
      addIfBlack = if color == Black then 1 else 0
  -- If the current node is black, then all the paths passing through this node should have black node count increased by 1
  in map (+ addIfBlack) (blackCountLeft ++ blackCountRight) -- Add addIfBlack to the concatenated list of left and right sub trees


-- Black height of a black-balanced tree, -1 if not black-balanced
blackHeight :: RBT a -> Int
blackHeight LeafRB = 0
blackHeight a
  | blackBalanced a = head (countBlackNodes a) -- countBlackNodes returns a list of the number of black nodes in each path, take only the head
  | otherwise = -1


-- Check if all Red-Black Tree conditions are satisfied
isRBT :: Ord a => RBT a -> Bool
isRBT a = isBST a && isRootBlack a && noConsecutiveRed a && blackBalanced a

-- Check if the root is black
isRootBlack :: RBT a -> Bool
isRootBlack (NodeRB Black _ _ _) = True
isRootBlack _ = False

-- Check that there are no consecutive red nodes
noConsecutiveRed :: RBT a -> Bool
noConsecutiveRed LeafRB = True
noConsecutiveRed (NodeRB color leftRBT _ rightRBT) = 
    noRedChildren color leftRBT && noRedChildren color rightRBT && noConsecutiveRed leftRBT && noConsecutiveRed rightRBT

-- Helper function to check for red children of a red node
noRedChildren :: Color -> RBT a -> Bool
noRedChildren Red (NodeRB Red _ _ _) = False --If parent node is red and left/right child node is also red, return False
noRedChildren _ _ = True


-- -- Insert a new element in a RBT, preserving the RBT properties

-- insertRB :: Ord a => a -> RBT a -> RBT a


-- -- Delete an element from a RBT, preserving the RBT properties

-- deleteRB :: Ord a => a -> RBT a -> RBT a


