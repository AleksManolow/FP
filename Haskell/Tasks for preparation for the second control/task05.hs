import Data.Char
main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD

reduceStr :: String -> String
reduceStr str = helper 0 str str
 where
     helper :: Int -> String -> String-> String
     helper _ [_] newStr = newStr 
     helper counter (x:y:xs) newStr
      | ((ord x - 32) == ord y || (ord x + 32) == ord y) = helper 0 (concat [(take counter newStr), xs])  (concat [(take counter newStr), xs]) 
      | otherwise = helper (counter + 1) (y:xs) newStr
 