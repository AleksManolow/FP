main :: IO()
main = do
    print $ getSquares 0 100 10 == [(0, 0), (10, 100), (20, 400), (30, 900), (40, 1600), (50, 2500), (60, 3600), (70, 4900), (80, 6400), (90, 8100), (100, 10000)]
    print $ getSquaresMap 0 100 10 == [(0, 0), (10, 100), (20, 400), (30, 900), (40, 1600), (50, 2500), (60, 3600), (70, 4900), (80, 6400), (90, 8100), (100, 10000)]

getSquares :: Int -> Int -> Int -> [(Int, Int)]
getSquares a b k = [ (x, x * x) | x <- [a, (a + k) .. b]] 

getSquaresMap :: Int -> Int -> Int -> [(Int, Int)]
getSquaresMap a b k = map (\ x -> (x, x * x)) [a, (a + k) .. b]