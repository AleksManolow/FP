main :: IO()
main = do
    print $ minDistance [(1, 2, 3), (2, 1, 4), (4, 2, 5), (0, 5, 4), (7, 8, 9)] == 3


minDistance :: [(Double,Double,Double)] -> Double
minDistance xs = minimum $  map (\x@(x1, y1, z1) -> (minimum $ map (\(x2, y2, z2) -> (x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)+(z1-z2)*(z1-z2) ) $ filter (\y -> y /= x) xs)) xs