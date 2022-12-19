main :: IO()
main = do
    print $ isTriangular [] == True
    print $ isTriangular [[-1]] == True
    print $ isTriangular [[-1, -2, -3], [0, -5, -6], [0, 0, -9]] == True
    print $ isTriangular [[1, 2, 3], [0, 5, 6], [0, 0, 9]] == True
    print $ isTriangular [[0, 2, 3], [0, 0, 6], [1, 0, 0]] == False
    print $ isTriangular [[1, 2, 3], [1, 5, 6], [0, 0, 9]] == False
    print $ isTriangular [[1, 2, 3, 4], [0, 5, 6, 7], [0, 0, 8, 9], [0, 0, 0, 9]] == True

isTriangular :: (Eq a, Num a) => [[a]] -> Bool
isTriangular [] = True
isTriangular xss = helper 0 xss
 where
      helper :: (Eq a, Num a) => Int -> [[a]] -> Bool
      helper _ [] = True
      helper i (xs:xss) = take i xs == replicate i 0 && helper (i + 1) xss