main :: IO()
main = do
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False
    --print $ isPalindrome (-654) 
    
rev :: Int -> Int 
rev x = helper 0 x
    where
        helper :: Int -> Int -> Int
        helper result 0 = result
        helper result leftOver = helper ((result * 10) + (mod leftOver 10)) (div leftOver 10)    

isPalindrome :: Int -> Bool
isPalindrome x 
  | x < 0 = error "x was negative"
  | otherwise = x == (rev x) 