main :: IO()
main = do
    print $ sumRats (2, 5) (5, 2) == (29, 10)
    print $ sumRats (52, 123) (96, 14) == (6268, 861)
    print $ sumRats (2, 5) (3, 5) == (1, 1)

    print $ multiplyRats (2, 5) (5, 2) == (1, 1)
    print $ multiplyRats (52, 123) (96, 14) == (832, 287)
    print $ multiplyRats (2, 5) (3, 5) == (6, 25)

    print $ divideRats (2, 5) (5, 2) == (4, 25)
    print $ divideRats (52, 123) (96, 14) == (91, 1476)
    print $ divideRats (2, 5) (3, 5) == (2, 3)

    print $ areEqual (2, 5) (5, 2) == False
    print $ areEqual (52, 123) (52 * 3, 123 * 3) == True
    print $ areEqual (2, 6) (5, 15) == True

type Rat a = (a, a)

sumRats :: (Integral a) => Rat a -> Rat a -> Rat a
sumRats (x1, y1) (x2, y2) = (div (x1 * y2 + x2 * y1) d,div (y1 * y2) d)
 where d = gcd (x1 * y2 + x2 * y1) (y1 * y2)

multiplyRats ::(Integral a)  => Rat a -> Rat a -> Rat a 
multiplyRats (x1, y1) (x2, y2) = (div (x1 * x2) d, div (y1 * y2) d)
 where d = gcd (x1 * x2) (y1 * y2)

divideRats ::(Integral a)  => Rat a -> Rat a -> Rat a 
divideRats (x1, y1) (x2, y2) = (div (x1 * y2) d, div (y1 * x2) d)
 where d = gcd (x1 * y2) (y1 * x2)

areEqual :: (Integral a) => Rat a -> Rat a -> Bool
areEqual (x1, y1) (x2, y2) = (div x1 d1, div y1 d1) == (div x2 d2, div y2 d2)
 where 
     d1 = gcd x1 y1
     d2 = gcd x2 y2

 
