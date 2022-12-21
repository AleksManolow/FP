import Data.List
main :: IO()
main = do
    print $ isSorted [-5, -5, -6] == True
    print $ isSorted [-5, -5, -4] == True
    print $ isSorted [1, 1, 1, 1, 1, 1, 1, 1, 1] == True
    print $ isSorted [1, 2, 3, 3, 3, 4, 5, 6, 6] == True
    print $ isSorted [1, -1, -3, -3, -3, -4, -5, -6, -6] == True
    print $ isSorted [1, 2, 3, 3, 3, 4, 5, 6, 5] == False
    print $ isSorted [-100, -99, -99, -99] == True
    print $ isSorted [-100, -99, -99, -99, 100] == True
    print $ isSorted [100, 101, -102] == False
    print $ isSorted [1, 2, 3, 4, 5, 6] == True
    print $ isSorted [-1, -2, -3, -4, -5, -6] == True

    print $ isSortedXs [-5, -5, -6] == True
    print $ isSortedXs [-5, -5, -4] == True
    print $ isSortedXs [1, 1, 1, 1, 1, 1, 1, 1, 1] == True
    print $ isSortedXs [1, 2, 3, 3, 3, 4, 5, 6, 6] == True
    print $ isSortedXs [1, -1, -3, -3, -3, -4, -5, -6, -6] == True
    print $ isSortedXs [1, 2, 3, 3, 3, 4, 5, 6, 5] == False
    print $ isSortedXs [-100, -99, -99, -99] == True
    print $ isSortedXs [-100, -99, -99, -99, 100] == True
    print $ isSortedXs [100, 101, -102] == False
    print $ isSortedXs [1, 2, 3, 4, 5, 6] == True
    print $ isSortedXs [-1, -2, -3, -4, -5, -6] == True


ascendingOrder :: [Int] -> Bool
ascendingOrder (_:[]) = True
ascendingOrder (x:y:xs) = (x <= y && ascendingOrder (y:xs))

descendingOrder :: [Int] -> Bool
descendingOrder (_:[]) = True
descendingOrder (x:y:xs) = (x >= y && descendingOrder (y:xs))

isSorted :: [Int] -> Bool
isSorted (x:y:xs) = (x <= y && ascendingOrder (y:xs)) || (x >= y && descendingOrder (y:xs))

isSortedXs :: [Int] -> Bool
isSortedXs xs = sort xs == xs || (reverse $ sort xs) == xs

