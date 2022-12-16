main :: IO()
main = do
    print $ reverseOrdSuff 37563 == 36
    print $ reverseOrdSuff 32763 == 367
    print $ reverseOrdSuff 32567 == 7
    print $ reverseOrdSuff 32666 == 6

reverseOrdSuff :: Int -> Int
reverseOrdSuff x = helper 0 x
 where
     helper :: Int -> Int -> Int
     helper result leftOver = if leftOver == 0 || mod leftOver 10 >= mod (div leftOver 10) 10 then result * 10 + (mod leftOver 10) else  helper (result * 10 + (mod leftOver 10)) (div leftOver 10)
    