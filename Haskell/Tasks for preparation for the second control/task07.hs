main :: IO()
main = do
    print $ inverseFun (\x -> x+1) (\x -> x-1) 5 10 == True
    print $ inverseFun (\x -> x*x) (\x -> x^3) 0 1 == True
    print $ inverseFun (\x -> x+1) (\x -> x+2) 0 1 == False

inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g a b = foldl1 (\x y -> x && y ) $ map (\x -> if f (g x) == g (f x) && f (g x) >= (min a b) && f (g x) <=(max a b) then True else False ) [(min a b) .. (max a b)]