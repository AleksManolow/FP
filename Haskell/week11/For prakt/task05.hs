main :: IO()
main = do
    print $ sumVectors (1, 2, 3) (4, 5, 6) == (5, 7, 9)
    print $ sumVectors (0, 0, 0) (100, 200, -300) == (100, 200, -300)

    print $ scaleVector (1, 2, 3) 5 == (5, 10, 15)
    print $ scaleVector (5, 2, 159) (-2) == (-10, -4, -318)

    print $ dotProduct (1, 2, 3) (7, 4, 1)
    print $ dotProduct (5, 2, 159) (0, -1, -2)

type Vector a = (a, a, a)

sumVectors :: (Num a) => Vector a -> Vector a -> Vector a
sumVectors (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

scaleVector :: (Num a) => Vector a -> a -> Vector a
scaleVector (x1, y1, z1) s = (s * x1, s * y1, s * z1)

dotProduct :: (Num a) => Vector a -> Vector a -> a
dotProduct (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2