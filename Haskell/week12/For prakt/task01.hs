main :: IO()
main = do
    print $ Circle 5.67
    print $ Rectangle 6 9.89
    print $ Triangle 5 6 7
    print $ Cylinder 78 109

data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Show)