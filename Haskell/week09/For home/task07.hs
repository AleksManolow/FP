main :: IO()
main = do
    print $ perfect 1 == False
    print $ perfect 6 == True
    print $ perfect 495 == False
    print $ perfect 33550336 == True

perfect :: Int -> Bool
perfect x = if x == sum [ n | n <- [1 .. (x - 1)], mod x n == 0] then True else False
