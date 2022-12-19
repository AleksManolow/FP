main :: IO()
main = do
    print $ normalize (4, 2) == (2, 1)
    print $ normalize (8, 4) == (2, 1)
    print $ normalize (2, 4) == (1, 2)
    print $ normalizeWhere (2, 4) == (1, 2)

type Rat = (Int, Int)

normalize :: Rat -> Rat
normalize (x, 0) = error "can't divide by zero"
normalize (x, y) = let d = gcd x y in (div x d, div y d)

normalizeWhere :: Rat -> Rat
normalizeWhere (x, 0) = error "can't divide by zero"
normalizeWhere (x, y) = (div x d, div y d)
 where d = gcd x y