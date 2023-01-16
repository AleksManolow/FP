main :: IO()
main = do
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] == [3, 4, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]
    
isLeaf :: [(Int, Int, Int)] -> Int -> Bool
isLeaf xs k = [] == filter (\(x, y, z) -> x == k) xs

listLeaves :: [(Int, Int, Int)] -> [Int]
listLeaves xs = foldl1 (\ x y -> x ++ y) $ map (\(x, y, z) -> (if isLeaf xs y then [y] else []) ++ (if isLeaf xs z then [z] else [])) xs