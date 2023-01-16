main :: IO()
main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2

getOddCompositionValue :: (Num a) =>[(a -> a)] -> a -> a 
getOddCompositionValue [] p = p 
getOddCompositionValue (x:y:xs) p = getOddCompositionValue xs (x p) 