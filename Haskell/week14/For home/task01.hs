main :: IO()
main = do
    print $ rangedSum firstTree 100 50 == 0 -- (L = 100, R = 50)
    print $ rangedSum firstTree 7 15 == 32 -- (L = 7, R = 15)
    print $ rangedSum firstTree 15 7 == 32 -- (L = 15, R = 7)
    print $ rangedSum secondTree 6 10 == 23 -- (L = 6, R = 10)
    print $ rangedSum secondTree 10 6 == 23 -- (L = 10, R = 6)

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show)


firstTree = Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 Nil (Node 18 Nil Nil))
secondTree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

rangedSum :: (Ord a, Num a) => BTree a -> a -> a -> a
rangedSum Nil _ _ = 0
rangedSum (Node value left right) a b
 |(value >= (min a b) && value <= (max a b)) = value + (rangedSum left a b) + (rangedSum right a b)
 |otherwise = (rangedSum left a b) + (rangedSum right a b)