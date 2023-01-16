main :: IO()
main = do
    print $ getAverage db1 == 4.457142857142857

    print $ getNeeded 750 db1 == [Product "Cheese" 750 5.0,Product "Water" 500 0.5,Product "Soap" 250 4.5]

    print $ closestToAverage db1 == ["Milk","Soap"]

    print $ cheaperAlternatives "Lamb" 5.50 db2 == 1
    print $ cheaperAlternatives "Lamb" 10  db2 == 2

type Name = String
type Quantity = Int
type Price = Double
type Database = [Product]

data Product = Product Name Quantity Price
 deriving(Show, Eq)

db1 :: Database
db1 = [ Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Butter" 1000 5.50, Product "Water" 500 0.50, Product "Soap" 250 4.50 ]

db2 :: Database
db2 = [ Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Lamb" 1000 5.50, Product "Water" 500 0.50, Product "Lamb" 250 4.50 ]


getAverage :: Database -> Price
getAverage xs = (sum $ map (\ (Product _ _ p) -> p) xs) / (fromIntegral $ length xs)

getNeeded :: Quantity -> Database -> Database
getNeeded x xs = filter (\ (Product _ q _) -> q <= x) xs

roundTwoDigButWithMagic :: Double -> Double
roundTwoDigButWithMagic = (/ 10) . fromIntegral . round . (*10)

closestToAverage :: Database -> [Name]
closestToAverage xs =  map (\ (Product q _ _) -> q) $ filter (\ (Product _ _ p) -> p == (roundTwoDigButWithMagic $ getAverage xs) ) xs

cheaperAlternatives :: Name -> Price -> Database -> Int
cheaperAlternatives n p xs = length $ filter (\ (Product pN _ pP) -> pN == n && pP < p) xs



