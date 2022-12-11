main :: IO()
main = do
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False

hasIncDigits :: Int -> Bool
hasIncDigits x
  | (x < 10) = True
  | (mod x 10 >= mod (div x 10) 10) = hasIncDigits (mod x 10)
  | otherwise = False
--hasIncDigits x = (x < 10) 
--               || (mod x 10 >= mod (div x 10) 10) && hasIncDigits (mod x 10)
