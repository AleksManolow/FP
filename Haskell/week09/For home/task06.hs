main :: IO()
main = do
    print $ isPrimeG 1 == False
    print $ isPrimeG 2 == True
    print $ isPrimeG 3 == True
    print $ isPrimeG 6 == False
    print $ isPrimeG 61 == True

    print $ isPrimeLC 1 == False
    print $ isPrimeLC 2 == True
    print $ isPrimeLC 3 == True
    print $ isPrimeLC 6 == False
    print $ isPrimeLC 61 == True

isPrimeG :: Int -> Bool
isPrimeG x =  x > 1 && helper 2
    where
        helper :: Int -> Bool
        helper counter = x == counter 
                        || mod x counter /= 0 && helper (counter + 1)

isPrimeLC :: Int -> Bool
isPrimeLC x  =  x > 1 && null [ n | n <- [2 .. (x - 1)], mod x n == 0]  
                   