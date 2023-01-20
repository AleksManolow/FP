import Data.List
main :: IO()
main = do
    print $ orderedTree treeOne == True
    print $ orderedTree treeTwo == False



data BTree a = NullT | Node a (BTree a) (BTree a)
 deriving (Show)

treeOne = Node (3.0,10.0) (Node (5.0,8.0) (Node (6.0,7.0) NullT NullT) (Node (4.0,9.0) NullT NullT)) (Node (2.0,12.0) NullT (Node (1.0,15.0) NullT NullT))

treeTwo = Node (3.0,10.0) (Node (5.0,8.0) (Node (6.0,7.0) NullT NullT) (Node (7.0,9.0) NullT NullT)) (Node (2.0,12.0) NullT (Node (1.0,15.0) NullT NullT))

transformTree :: (Num a) => BTree(a, a) -> BTree a
transformTree NullT = NullT
transformTree (Node (x, y) left right) = Node (y - x) (transformTree left) (transformTree right)

traverseDFS :: BTree a -> [a]
traverseDFS NullT = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

orderedTree t = let nodes = (traverseDFS $ transformTree t) in nodes == sort nodes