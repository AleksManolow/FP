main :: IO()
main = do
    print $ myGcd 5 13 == 1
    print $ myGcd 13 1235 == 13

myGcd :: Int -> Int -> Int
myGcd 0 y = y
myGcd x 0 = x
myGcd x y 
  | mod (max x y) (min x y) == 0 = min x y 
  | otherwise = myGcd (min x y) (mod (max x y) (min x y))