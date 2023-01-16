main :: IO()
main = do
    print $ constructMaxBTree [3, 2, 1, 6, 0, 5] == t2

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show)

t2 = Node 6 (Node 3 Nil (Node 2 Nil (Node 1 Nil Nil))) (Node 5 (Node 0 Nil Nil) Nil)


constructMaxBTree :: (Ord a) => [a] -> BTree a
constructMaxBTree [] = Nil 
constructMaxBTree xs = Node (maxInXs) (constructMaxBTree  (takeWhile (<maxInXs) xs) ) (constructMaxBTree (dropWhile (<maxInXs) xs))
 where maxInXs = maximum xs