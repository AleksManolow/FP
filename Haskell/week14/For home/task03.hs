main :: IO()
main = do
    print $ toBinaryIndexed tree == Node (10,5) (Node (5,2) (Node (3,1) (Node (1,0) Nil Nil) Nil) (Node (7,4) (Node (6,3) Nil Nil) Nil)) (Node (15,7) (Node (13,6) Nil Nil) (Node (18,8) Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

tree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

onlyNodes Nil = []
onlyNodes (Node value left right) = onlyNodes left ++ [value] ++ onlyNodes right 

toBinaryIndexed Nil = Nil
toBinaryIndexed tree = helper (zip (onlyNodes tree) [0 ..]) tree
 where
     helper _ Nil = Nil
     helper xs (Node value left right) = Node (value, head [ idx | (el, idx) <- xs, el == value]) (helper xs left) (helper xs right)