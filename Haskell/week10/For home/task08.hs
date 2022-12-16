import Data.Char
main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
    
reduceStr :: String -> String
reduceStr str = helper 0 str str
 where
     helper :: Int -> String -> String-> String 
     helper counter xs newStr
        | ((length xs) == 1) = newStr
        | ((ord (head xs) - 32) == ord (head (tail xs)) || (ord (head xs) + 32) == ord (head (tail xs))) = helper 0 (concat [(take counter newStr), (tail (tail xs))])  (concat [(take counter newStr), (tail (tail xs))]) 
        | otherwise = helper (counter + 1) (tail xs) newStr
 