main :: IO()
main = do
    print $ isInsideNoLists 5 1 4 == True
    print $ isInsideNoLists 10 50 20 == True
    print $ isInsideNoLists 10 50 1 == False

    print $ isInsideLists 5 1 4 == True
    print $ isInsideLists 10 50 20 == True
    print $ isInsideLists 10 50 1 == False

    print $ isInsideLambda 5 1 4 == True
    print $ isInsideLambda 10 50 20 == True
    print $ isInsideLambda 10 50 1 == False

isInsideNoLists :: Int -> Int -> Int -> Bool
isInsideNoLists a b x = if(min a b) <= x && (max a b ) >= x then True else False

isInsideLists :: Int -> Int -> Int -> Bool
isInsideLists a b x = elem x [(min a b) .. (max a b )]

isInsideLambda :: Int -> Int -> Int -> Bool
isInsideLambda a b = (\ x -> min a b <= x && max a b >= x )
