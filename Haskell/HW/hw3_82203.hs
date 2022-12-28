main :: IO()
main = do

    print $ willItFly [1, 4, 2, 3] == True -- |1-4|=3,|4-2|=2,|2-3|=1
    print $ willItFly [1, 4, 2, -1, 6]  == False
    print $ willItFly [3, -1, -2, 1, 3] == True

    print $ formatDuration 0 == "now"
    print $ formatDuration 1 == "1 second"
    print $ formatDuration 62 == "1 minute and 2 seconds"
    print $ formatDuration 120 == "2 minutes"
    print $ formatDuration 3600 == "1 hour"
    print $ formatDuration 3662 == "1 hour, 1 minute and 2 seconds"
    print $ formatDuration 120078984 == "3 years, 294 days, 19 hours, 16 minutes and 24 seconds"
    print $ formatDuration 61 == "1 minute and 1 second"
    --print $ formatDuration (-5) 

--task 01
willItFly :: [Int] -> Bool
willItFly xs = helper xs [1 .. (length xs) - 1]
 where
     helper :: [Int] -> [Int] -> Bool
     helper [_] _ = True
     helper (x:y:xs) ys  =  elem (abs (x - y)) ys && helper (y:xs) [ a | a <- ys, (abs (x - y)) /= a ]

--task 02     
appendAndOrComma ::Int -> Int -> Int-> String
appendAndOrComma n leftOver modNumber
    | (n /= leftOver && modNumber == 0) = " and "
    | (n /= leftOver && modNumber /= 0) = ", "
    | otherwise = []


formatDuration :: Int -> String
formatDuration 0 = "now"
formatDuration n = if n < 0 then error "Invalid value" else helper n
 where
     helper :: Int -> String
     helper 0 = []
     helper sec 
        | (sec >= 31536000 && (div sec 31536000) == 1) = show (div sec 31536000) ++  " year" ++ helper (mod sec 31536000)
        | (sec >= 31536000 && (div sec 31536000) /= 1) = show (div sec 31536000) ++  " years" ++ helper (mod sec 31536000)
        | (sec >= 86400 && (div sec 86400) == 1) = appendAndOrComma n sec (mod sec 86400) ++ show (div sec 86400) ++  " day" ++ helper (mod sec 86400)
        | (sec >= 86400 && (div sec 86400) /= 1) = appendAndOrComma n sec (mod sec 86400) ++ show (div sec 86400) ++  " days" ++ helper (mod sec 86400)
        | (sec >= 3600 && (div sec 3600) == 1) = appendAndOrComma n sec (mod sec 3600) ++ show (div sec 3600) ++  " hour" ++ helper (mod sec 3600)
        | (sec >= 3600 && (div sec 3600) /= 1) = appendAndOrComma n sec (mod sec 3600) ++ show (div sec 3600) ++  " hours" ++ helper (mod sec 3600)
        | (sec >= 60 && (div sec 60) == 1) = appendAndOrComma n sec (mod sec 60) ++ show (div sec 60) ++  " minute" ++ helper (mod sec 60)
        | (sec >= 60 && (div sec 60) /= 1) = appendAndOrComma n sec (mod sec 60) ++ show (div sec 60) ++  " minutes" ++ helper (mod sec 60)
        | (sec == 1) = appendAndOrComma n sec 0 ++ show sec ++ " second" ++ helper 0 
        | otherwise = appendAndOrComma n sec 0 ++ show sec ++ " seconds" ++ helper 0          
        