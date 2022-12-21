main :: IO()
main = do
    print $ naturalProduct [-1, 0, -2, -3] 5 == 0 -- There are no natural numbers
    print $ naturalProduct [5, 10] 5 == 0 -- Sum of the divisors for 5 is 1 and for 10 is 1+2+5=8
    print $ naturalProduct [95, 75, 15, 55, 11, 14, 18, 35, 25] 5 == 1330
     

sumOfDividers :: Int -> Int 
sumOfDividers n =  sum $ [ x | x <- [1 .. (n - 1)], mod n x == 0 ]

naturalProduct :: [Int] -> Int -> Int
naturalProduct xs k = if [ x | x <- xs, mod (sumOfDividers x) k == 0] == [] then 0 else (product $ [ x | x <- xs, mod (sumOfDividers x) k == 0])