import Data.List
main :: IO()
main = do
    print $ closestToAverage store1 == "cheese"
    print $ cheaperAlternative store2 == 1 



type Product = (String,Double)
type StoreAvailability = [Product]

store1 = [("bread",1),("milk",2.5),("lamb",10),("cheese",5),("butter",2.3)]

store2=[("bread",1),("cheese",2.5),("bread",1),("cheese",5),("butter",2.3)]

closestToAverage :: StoreAvailability -> String
closestToAverage xs = fst $ foldl1 (\ s1@(p1, v1) s2@(p2, v2) -> if (abs (v1 - avarageVlue)) < (abs (v2 - avarageVlue)) then s1 else s2) xs
 where avarageVlue = (sum $ map (\ (_, v) -> v) xs) / (fromIntegral $ length xs) 

cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative [] = 0
cheaperAlternative k@((p, v):xs)
 | (length $ filter (\(x, y) -> x == p && y /= v) xs) /= 0 = 1 + cheaperAlternative (filter (\(x, _) -> x /= p ) k)
 | otherwise = cheaperAlternative (filter (\(x, _) -> x /= p ) k)