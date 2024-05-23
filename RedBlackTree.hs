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

-- Insert a new element in a RBT, preserving the RBT properties
insertRB :: Ord a => a -> RBT a -> RBT a
insertRB a tree = blackRoot (ins a tree)

-- Function to paint the root node black
blackRoot :: RBT a -> RBT a
blackRoot LeafRB = LeafRB
blackRoot (NodeRB _ leftRBT value rightRBT) = NodeRB Black leftRBT value rightRBT

-- Helper function for insert
ins :: Ord a => a -> RBT a -> RBT a
ins a LeafRB = NodeRB Red LeafRB a LeafRB
ins a t@(NodeRB color t1 x t2)
  | a < x = balance color (ins a t1) x t2
  | a > x = balance color t1 x (ins a t2)
  | otherwise = t

-- Function to balance the tree based on pattern matching
balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance Black (NodeRB Red (NodeRB Red t1 x1 t2) x2 t3) x3 t4 = -- Left-Left case
  NodeRB Red (NodeRB Black t1 x1 t2) x2 (NodeRB Black t3 x3 t4)
balance Black t1 x1 (NodeRB Red t2 x2 (NodeRB Red t3 x3 t4)) = -- Right-Right case
  NodeRB Red (NodeRB Black t1 x1 t2) x2 (NodeRB Black t3 x3 t4) 
balance Black (NodeRB Red t1 x1 (NodeRB Red t2 x2 t3)) x3 t4 = -- Left-Right case
  NodeRB Red (NodeRB Black t1 x1 t2) x2 (NodeRB Black t3 x3 t4)
balance Black t1 x1 (NodeRB Red (NodeRB Red t2 x2 t3) x3 t4) = -- Right-Left case 
  NodeRB Red (NodeRB Black t1 x1 t2) x2 (NodeRB Black t3 x3 t4)
balance color t1 x t2 =  NodeRB color t1 x t2 -- For all other patterns

-- Delete an element from a RBT, preserving the RBT properties
deleteRB :: Ord a => a -> RBT a -> RBT a
deleteRB x t = blackRoot (del x t)

-- Function to rebalance the RBT when hte left child has height smaller than 1
balL :: RBT a -> a -> RBT a -> RBT a
balL (NodeRB Red t1 x t2) y t3 = NodeRB Red (NodeRB Black t1 x t2) y t3
balL t1 y (NodeRB Black t2 z t3) = balance Black t1 y (NodeRB Red t2 z t3)
balL t1 y (NodeRB Red (NodeRB Black t2 u t3) z t4) = 
  NodeRB Red (NodeRB Black t1 y t2) u (balance Black t3 z (redRoot t4))

-- Function to rebalance the RBT when the right child has height smaller than 1
balR :: RBT a -> a -> RBT a -> RBT a
balR t1 y (NodeRB Red t2 x t3) = NodeRB Red t1 y (NodeRB Black t2 x t3)
balR (NodeRB Black t1 z t2) y t3 = balance Black (NodeRB Red t1 z t2) y t3
balR (NodeRB Red t1 z (NodeRB Black t2 u t3)) y t4 = 
  NodeRB Red (balance Black (redRoot t1) z t2) u (NodeRB Black t3 y t4)

-- Function to paint the root node red
redRoot :: RBT a -> RBT a
redRoot LeafRB = LeafRB
redRoot (NodeRB _ leftRBT value rightRBT) = NodeRB Red leftRBT value rightRBT

-- A function that retrieves the color of the tree
color :: RBT a -> Color
color LeafRB = Black -- Leaves are considered black
color (NodeRB c _ _ _) = c

-- Function to delete a key from the left subtree
delL :: Ord a => a -> RBT a -> a -> RBT a -> RBT a
delL x t1 y t2 =
  if color t1 == Black
  then balL (del x t1) y t2
  else NodeRB Red (del x t1) y t2

-- Function to delete a key from the right subtree
delR :: Ord a => a -> RBT a  -> a -> RBT a  -> RBT a 
delR x t1 y t2 =
  if color t2 == Black
  then balR t1 y (del x t2)
  else NodeRB Red t1 y (del x t2)

del :: Ord a => a -> RBT a -> RBT a
del x LeafRB = LeafRB
del x (NodeRB _ t1 y t2)
  | x < y = delL x t1 y t2 -- delete from left child
  | x > y = delR x t1 y t2 -- delete from right child
  | otherwise = fuse t1 t2 -- delete root, fuse children

-- Function to fuse two RBTs together
fuse :: RBT a -> RBT a -> RBT a
fuse LeafRB t2 = t2
fuse t1 LeafRB = t1
fuse t1@(NodeRB Black _ _ _) (NodeRB Red t3 y t4) = NodeRB Red (fuse t1 t3) y t4 -- Fuse balck and red tree
fuse (NodeRB Red t1 x t2) t3@(NodeRB Black _ _ _) = NodeRB Red t1 x (fuse t2 t3) -- Fuse red and black tree
fuse (NodeRB Red t1 x t2) (NodeRB Red t3 y t4)  = -- Fuse two red trees together
  let s = fuse t2 t3 -- Recursively fuse t2 and t3
  in case s of
       (NodeRB Red s1 z s2) -> (NodeRB Red (NodeRB Red t1 x s1) z (NodeRB Red s2 y t4))
       (NodeRB Black _ _ _) -> (NodeRB Red t1 x (NodeRB Red s y t4))
       LeafRB -> NodeRB Red t1 x (NodeRB Red LeafRB y t4) -- Treat leaf case similar to black node after fusing t2 and t3
fuse (NodeRB Black t1 x t2) (NodeRB Black t3 y t4)  = -- Fuse two black trees together
  let s = fuse t2 t3 -- Recursively fuse t2 and t3
  in case s of
       (NodeRB Red s1 z s2) -> (NodeRB Red (NodeRB Black t1 x s1) z (NodeRB Black s2 y t4))
       (NodeRB Black _ _ _) -> balL t1 x (NodeRB Black s y t4)
       LeafRB -> balL t1 x (NodeRB Black LeafRB y t4) -- Treat leaf case similar to black node after fusing t2 and t3