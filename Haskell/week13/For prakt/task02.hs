main :: IO()
main = do
    print $ db
    print $ getTotal db == 31.2
    print $ getTotalMap db == 31.2

    print $ "Buying 500 Bread:"
    print $ buy "Bread" 500 db
    print $ "Buying 500 Water:"
    print $ buy "Water" 500 $ buy "Bread" 500 db
    print $ "Buying 100 Soap:"
    print $ buy "Soap" 100 $ buy "Water" 500 $ buy "Bread" 500 db
    print $ "Buying 500 Bread:"
    print $ buy "Bread" 500 $ buy "Soap" 100 $ buy "Water" 500 $ buy "Bread" 500 db
    -- print $ buy "Water" 100 $ buy "Bread" 500 $ buy "Soap" 100 $ buy "Water" 500 $ buy "Bread" 500 db -- Should give an error.
    -- print $ buy "Soap" 200 $ buy "Bread" 500 $ buy "Soap" 100 $ buy "Water" 500 $ buy "Bread" 500 db -- Should give an error.

type Name = String
type Quantity = Int
type Price = Double
type Database = [Product]

data Product = Product Name Quantity Price
 deriving (Show)

db = [Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Butter" 1000 5.50, Product "Water" 500 0.50, Product "Soap" 250 4.50]

getTotal :: Database -> Price
getTotal = foldl (\ acc (Product _ _ p) -> p + acc) 0

getTotalMap :: Database -> Price
getTotalMap = sum . map (\ (Product _ _ p) -> p)

buy :: Name -> Quantity -> Database -> Database
buy name _ [] = error $ "ERROR: No " ++ name ++ " in store!"
buy name quantity (p@(Product pName pQuantity pPrice):ps)
 | name == pName && quantity < pQuantity = (Product pName (pQuantity - quantity) pPrice):ps
 | name == pName && quantity == pQuantity = ps
 | name == pName = error $ "ERROR: Not enough " ++ pName ++ " in store!"
 | otherwise = p : buy name quantity ps