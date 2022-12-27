main :: IO()
main = do
    print $ TwoD 6 7
    print $ ThreeD 6 7 8.43
    print $ getPointsHOF (\x -> x * x) (2+) [TwoD 2 2, TwoD 1 2, TwoD 3 7] == [TwoD 2 2, TwoD 3 7]
    print $ getPointsLC (\x -> x * x) (2+) [TwoD 2 2, TwoD 1 2, TwoD 3 7] == [TwoD 2 2, TwoD 3 7]

data Point a = TwoD a a | ThreeD a a a
 deriving (Show, Eq)

etPointsHOF :: (Eq a) => (a -> a) -> (a -> a) -> [Point a] -> [Point a]
getPointsHOF f g = filter (\ (TwoD firstCoord secondCoord) -> f firstCoord == g secondCoord)

getPointsLC :: (Eq a) => (a -> a) -> (a -> a) -> [Point a] -> [Point a]
getPointsLC f g xs = [ x | x@(TwoD firstCoord secondCoord) <- xs, f firstCoord == g secondCoord] -- [(f, s) for (f, s) in xs if f f == g s]