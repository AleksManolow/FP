import Data.List
main :: IO()
main = do
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)] == "English"
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 5), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 5)] == "Maths"

type Student = String
type Subject = String
type Note = Double
type Record = (Student, Subject, Note)    

sumNote :: [Record] -> Subject -> Double
sumNote xs s = sum $ map (\ (_, _, z) -> z) $ filter (\ (_, y, z) -> y == s) xs  

sizeNote :: [Record] -> Subject -> Int
sizeNote xs s = length $ filter (\ (_, y, z) -> y == s) xs 

hardestSubject :: [Record] -> Subject
hardestSubject xs = fst $ foldl1 (\p1@(x1, y1) p2@(x2, y2) -> if y1 < y2 then p1 else p2) $  map (\ subj ->(subj, (sumNote xs subj) / (fromIntegral $ (sizeNote xs subj))) ) $ nub $ map (\(x, y, z) -> y) xs