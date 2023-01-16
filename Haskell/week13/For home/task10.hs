import Data.List
main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show)

t1 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (4, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))
t2 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (7, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

transformTree :: (Num a) => BTree(a, a) -> BTree a
transformTree Nil = Nil
transformTree (Node (x, y) left right) = Node (y - x) (transformTree left) (transformTree right)

traverseDFS :: BTree a -> [a]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

ordered t = let nodes = (traverseDFS $ transformTree t) in nodes == sort nodes
