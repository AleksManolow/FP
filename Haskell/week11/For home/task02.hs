main :: IO()
main = do
    print $ removeFirst 5 [5, 1, 5, 3, 5] == [1, 5, 3, 5]
    print $ removeFirst 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst s (x:xs)
    | s == x = xs
    | otherwise = x : removeFirst s xs