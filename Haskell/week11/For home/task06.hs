main :: IO()
main = do
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 5] == (True, 1)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 7] == (True, 3)
    print $ isImage [4, 5, 6, 7] [1, 2, 3, 4] == (True, -3)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 6] == (False, 0)
    print $ isImage [1, 2] [-1, -2] == (False, 0)
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 4] == (False, 0)

isImage :: [Int] -> [Int] -> (Bool, Int)   
isImage (x:xs) (y:ys) = helper xs ys (y - x) 
 where
     helper :: [Int] -> [Int] -> Int -> (Bool, Int)    
     helper [] [] n = (True, n)
     helper (x:xs) (y:ys) n 
        | (x + n == y) = helper xs ys n
        | otherwise = (False, 0)
