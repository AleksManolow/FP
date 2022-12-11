main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

sumOfDel :: Int -> Int 
sumOfDel x = helper 0 1
    where
        helper :: Int -> Int -> Int
        helper sum counter
          | counter >= x = sum
          | mod x counter == 0 = helper (sum + counter) (1 + counter) 
          | otherwise = helper sum (1 + counter)

areAmicable :: Int -> Int -> Bool
areAmicable x y = y == (sumOfDel x)