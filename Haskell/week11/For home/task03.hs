main :: IO()
main = do
    print $ removeAll 5 [5] == []
    print $ removeAll 4 [4, 4] == []
    print $ removeAll 5 [1] == [1]
    print $ removeAll 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAll 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]
    
    print $ removeAllHOF 5 [5] == []
    print $ removeAllHOF 4 [4, 4] == []
    print $ removeAllHOF 5 [1] == [1]
    print $ removeAllHOF 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAllHOF 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

removeAll :: (Eq a) => a -> [a] -> [a]
removeAll _ [] = []
removeAll s (x:xs) 
    | s == x = removeAll s xs
    | otherwise = x : removeAll s xs

removeAllHOF :: (Eq a) => a -> [a] -> [a]
removeAllHOF s xs = filter (\ x -> x /= s) xs

   
