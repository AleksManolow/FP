main :: IO()
main = do
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45 
    

sumEl :: [Int] -> [Int] -> Int
sumEl [] _ = 0
sumEl (x:xs) leftOver
 |((length leftOver) - 1 == length (filter(\ xp -> xp /= x) leftOver)) = x + sumEl xs leftOver
 |otherwise = sumEl xs leftOver


sumUnique :: [[Int]] -> Int
sumUnique [] = 0
sumUnique (xs:xxs) = sumEl xs xs + sumUnique xxs
