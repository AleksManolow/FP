main :: IO()
main = do
    print $ height numberBTree == 4
    print $ height charBTree == 3

    print $ average numberBTree == 16.22
    --print $ average charBTree -- should not work

    
    print $ sumLeaves numberBTree == 119
    --print $ sumLeaves charBTree -- should not work

    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil Nil))) == False
    print $ areEqual charBTree charBTree == True
    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))) == False

    print $ setLevels numberBTree == Node (0,5) (Node (1,12) (Node (2,1) (Node (3,96) Nil Nil) Nil) (Node (2,0) Nil Nil)) (Node (1,4) (Node (2,2) Nil Nil) (Node (2,5) Nil (Node (3,21) Nil Nil)))
    print $ setLevels charBTree == Node (0,'k') (Node (1,'a') (Node (2,'h') Nil Nil) (Node (2,'s') Nil Nil)) (Node (1,'l') (Node (2,'e') Nil Nil) (Node (2,'l') Nil Nil))

    print $ mirrorTree numberBTree == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
    print $ mirrorTree charBTree == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))



data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))

getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value _ _ ) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1) 

height :: BTree a -> Int
height t = length $ takeWhile (not . null) [ getLevel t k | k <- [0 ..]]

size :: BTree a -> Int
size Nil = 0;
size (Node value left right) = 1 + size left + size right

sumTree :: (Num a) => BTree a -> a 
sumTree Nil = 0
sumTree (Node value left right) = value + sumTree left + sumTree right

roundTwoDig :: Double -> Double
roundTwoDig x = (fromIntegral $ round $ x * 100) / 100

average ::(Num a, Integral a) => BTree a -> Double
average t = roundTwoDig $ (fromIntegral $ sumTree t) / (fromIntegral $ size t)

sumLeaves :: (Num a) => BTree a -> a
sumLeaves Nil = 0
sumLeaves (Node value Nil Nil) = value
sumLeaves (Node _ left right) = sumLeaves left + sumLeaves right

areEqual :: (Eq a) => BTree a -> BTree a -> Bool
areEqual = (==)

setLevels :: BTree a -> BTree (Int, a)
setLevels t = helper t 0
 where
     helper :: BTree a -> Int -> BTree (Int, a)
     helper Nil _ = Nil
     helper (Node value left right) k = Node (k, value) (helper left (k + 1)) (helper right (k + 1))

mirrorTree :: BTree a -> BTree a 
mirrorTree Nil = Nil
mirrorTree (Node value left right) = Node (value) (mirrorTree right) (mirrorTree left)




