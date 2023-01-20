import Data.List
main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show)

numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

levelSum :: (Num a) => BTree a -> Int -> a
levelSum Nil _ = 0
levelSum (Node value _ _ ) 0 = value
levelSum (Node value left right) k = levelSum left (k - 1) + levelSum right (k - 1) 

cone :: (Num a, Eq a, Ord a) => BTree a -> Bool
cone t = takeWhile (/= 0) [ levelSum t k | k <- [0 ..]] == (sort $ takeWhile (/= 0) [ levelSum t k | k <- [0 ..]]) 
