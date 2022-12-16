main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462
    
containDigit :: Int -> Int -> Bool 
containDigit num d
    | (num == 0) = False
    | (mod num 10 == d) = True
    | otherwise = containDigit (div num 10) d

isPrime :: Int -> Bool
isPrime x  =  x > 1 && null [ n | n <- [2 .. (x - 1)], mod x n == 0]  

sumSpecialPrimes :: Int -> Int ->Int
sumSpecialPrimes n d = helper 0 n
 where
     helper :: Int -> Int -> Int
     helper counter 0 = 0
     helper counter leftOver
        | (containDigit counter d && isPrime counter) = counter + helper (counter + 1) (leftOver - 1)
        | otherwise = helper (counter + 1) leftOver

          