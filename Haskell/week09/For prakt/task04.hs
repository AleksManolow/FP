main :: IO()
main = do
    print $ fibRec 11 == 89
    print $ fibIter 11 == 89
    print $ fibIter 110 == 43566776258854844738105

fibRec :: Int -> Int
fibRec 0 = 0
fibRec 1 = 1
fibRec x = fibRec (x - 1) + fibRec (x - 2)   

fibIter :: Integer -> Integer
fibIter x = helper 0 1 x
  where
      helper :: Integer -> Integer -> Integer -> Integer
      helper towAgo oneAgo 0 = towAgo
      helper towAgo oneAgo 1 = oneAgo
      helper towAgo oneAgo leftOver = helper oneAgo (oneAgo + towAgo) (leftOver - 1)