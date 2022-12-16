main :: IO()
main = do
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45 
     

sumEl :: [Int] -> [Int] -> Int
sumEl xs leftOver
    | (null leftOver) = 0
    | ((length xs) - 1 == length (filter (\ x -> x /= (head leftOver)) xs)) = (head leftOver) + sumEl xs (tail leftOver)
    | otherwise = sumEl xs (tail leftOver)

sumUnique :: [[Int]] -> Int
sumUnique xss = if null xss then 0 else sumEl (head xss) (head xss) + sumUnique (tail xss)
