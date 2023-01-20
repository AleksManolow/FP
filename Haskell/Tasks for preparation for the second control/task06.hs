main :: IO()
main = do
    print $ (maximize [(\x -> x * x * x),(\x -> x + 1)]) 0.5 -- == 1.5




maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
maximize xs = foldl1 (\f1 f2 -> if f1 > f2 then f1 else f2) xs