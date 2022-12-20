main :: IO()
main = do
    print $ combine [(1, 2), (1, 2)] == (11, 22)
    print $ combine [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)] == (377802, 989859)

combine :: (Ord a, Num a)  => [(a, a)] -> (a, a)
combine xss = helper (0, 0) xss
 where
     helper :: (Ord a, Num a) => (a, a) -> [(a, a)] -> (a, a)    
     helper (a, b) [] = (a, b)
     helper (a, b) ((x, y):xs) = helper (a * 10 + (min x y), b * 10 + (max x y)) xs