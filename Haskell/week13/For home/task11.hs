main :: IO()
main = do
    print $ colourBTree

    print $ highest Red colourBTree == 4
    print $ highest Green colourBTree == 3
    print $ highest Blue colourBTree == 4


data Color = Red | Blue | Green

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show)

colourBTree = Node Red (Node Green (Node Blue (Node Red Nil Nil) Nil) (Node Blue Nil Nil)) (Node Red (Node Green (Node Blue Nil Nil) Nil) (Node Red Nil Nil))

setLevels :: BTree a -> [(Int, a)]
setLevels t = helper t 1
 where
     helper :: BTree a -> Int -> [(Int, a)]
     helper Nil _ = []
     helper (Node value left right) k = [(k, value)] ++ (helper left (k + 1)) ++ (helper right (k + 1))

highest :: (Eq a) => Color -> BTree a -> Int
highest col t =  maximum $ map (\ (x, y) -> x) $ filter (\ (x, y) -> y == col) $ setLevels t