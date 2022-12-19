main :: IO()
main = do
    print $ sumTupleNoPM (4, 5) == 9
    print $ sumTupleNoPM (-5, 5) == 0

    print $ sumTuplePM (4, 5) == 9
    print $ sumTuplePM (-5, 5) == 0

    print $ (\ (x, y) -> x + y ) (4, 5) == 9
    print $ (\ (x, y) -> x + y ) (-5, 5) == 0

sumTupleNoPM :: (Num a) => (a, a) -> a
sumTupleNoPM p = fst p + snd p

sumTuplePM :: (Num a) => (a, a) -> a
sumTuplePM (x, y) = x + y

getAddOneLambda :: (Num a) => (a -> a)
getAddOneLambda = (\ x -> x + 1)