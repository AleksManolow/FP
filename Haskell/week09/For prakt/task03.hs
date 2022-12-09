main :: IO()
main = do
    print $ fact 11 == 39916800
    print $ factIf 11 == 39916800
    --print $ fact (-11) -- error: x was negative
    -- print $ factXs 11 == 39916800
    print $ factIter 11 == 39916800
    -- print $ factIter (-11) -- error: x was negative

fact :: Integer -> Integer
fact 0 = 1
fact x 
  | x < 0 = error "x was negative"
  | otherwise = x * fact (x - 1)    

factIf :: Int -> Int
factIf x = if x == 0 then 1 else x * factIf (x - 1)

factIter :: Int -> Int
factIter x 
  | x < 0 = error "x was negative"
  | otherwise = helper x 1
  where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftOver result = helper (leftOver - 1) (result * leftOver)

  