main :: IO()
main = do
    print $ genWords t1 == ["acf","acd","abe","cf","cd","f","d","be","e"]
    print $ genWords t2 == ["acd","ab","cd","d","b"]
    print $ genWords t3 == ["abdh","abdi","abe","acf","acg","bdh","bdi","be","dh","di","h","i","e","cf","cg","f","g"]

data BTree = Nil | Node Char BTree BTree

t1 :: BTree
t1 = Node 'a' (Node 'c' (Node 'f' Nil Nil) (Node 'd' Nil Nil)) (Node 'b' Nil (Node 'e' Nil Nil))

t2 :: BTree
t2 = Node 'a' (Node 'c' (Node 'd' Nil Nil) Nil) (Node 'b' Nil Nil)

t3 :: BTree
t3 = Node 'a' (Node 'b' (Node 'd' (Node 'h' Nil Nil) (Node 'i' Nil Nil)) (Node 'e' Nil Nil)) (Node 'c' (Node 'f' Nil Nil) (Node 'g' Nil Nil))

containsWord :: BTree -> String -> Bool
containsWord Nil _ = False
containsWord _ [] = False
containsWord (Node c Nil Nil) [x] = c == x
containsWord (Node val left right) temp@(x:xs)
 | val == x = helper left xs || helper right xs
 | otherwise = containsWord left temp || containsWord right temp
 where
     helper :: BTree -> String -> Bool
     helper Nil _ = False
     helper _ [] = False
     helper (Node c Nil Nil) [x] = c == x
     helper (Node val left right) (x:xs) = val == x && (helper left xs || helper right xs)

genWords :: BTree -> [String]
genWords Nil = [""]
genWords (Node word Nil Nil) = [[word]]
genWords tree@(Node word left right) = filter (containsWord tree)
                    $ (map (word:) $ genWords left ++ genWords right)
                    ++ genWords left
                    ++ genWords right