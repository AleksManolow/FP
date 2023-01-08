main :: IO()
main = do
    print $ getSunk database == [("Guadalcanal",["Kirishima"]),("North Atlantic",["Bismarck","Hood"]),("North Cape",["Schamhorst"]),("Surigao Strait",["Fuso","Yamashiro"])]
    --print $ inBattleAfterDamaged database -- == ["California","Prince of Wales"]

    print $ grandchildrenIncreased t1 == True
    print $ grandchildrenIncreased t2 == False

--task01
type Name = String
type Date = String
type Class = String
type Result = String
type Launched = Int

data Battle = Battle Name Date 
 deriving (Show)
data Ship = Ship Name Class Launched 
 deriving (Show)
data Outcome = Outcome Name Name Result 
 deriving (Show)

type Database = ([Outcome], [Battle], [Ship])

outcomes :: [Outcome]
outcomes = [ Outcome "Bismarck" "North Atlantic" "sunk", Outcome "California" "Surigao Strait" "ok", Outcome "Duke of York" "North Cape" "ok", Outcome "Fuso" "Surigao Strait" "sunk", Outcome "Hood" "North Atlantic" "sunk", Outcome "King George V" "North Atlantic" "ok", Outcome "Kirishima" "Guadalcanal" "sunk", Outcome "Prince of Wales" "North Atlantic" "damaged", Outcome "Rodney" "North Atlantic" "ok", Outcome "Schamhorst" "North Cape" "sunk", Outcome "South Dakota" "Guadalcanal" "damaged", Outcome "Tennessee" "Surigao Strait" "ok", Outcome "Washington" "Guadalcanal" "ok", Outcome "Prince of Wales" "Guadalcanal" "ok", Outcome "West Virginia" "Surigao Strait" "ok", Outcome "Yamashiro" "Surigao Strait" "sunk", Outcome "California" "Guadalcanal" "damaged" ]

battles :: [Battle]
battles = [ Battle "Guadalcanal" "1942-11-15", Battle "North Atlantic" "1941-05-25", Battle "North Cape" "1943-12-26", Battle "Surigao Strait" "1944-10-25" ]

ships :: [Ship]
ships = [ Ship "California" "Tennessee" 1921, Ship "Haruna" "Kongo" 1916, Ship "Hiei" "Kongo" 1914, Ship "Iowa" "Iowa" 1943, Ship "Kirishima" "Kongo" 1915, Ship "Kongo" "Kongo" 1913, Ship "Missouri" "Iowa" 1944, Ship "Musashi" "Yamato" 1942, Ship "New Jersey" "Iowa" 1943, Ship "North Carolina" "North Carolina" 1941, Ship "Ramillies" "Revenge" 1917, Ship "Renown" "Renown" 1916, Ship "Repulse" "Renown" 1916, Ship "Resolution" "Renown" 1916, Ship "Revenge" "Revenge" 1916, Ship "Royal Oak" "Revenge" 1916, Ship "Royal Sovereign" "Revenge" 1916, Ship "Tennessee" "Tennessee" 1920, Ship "Washington" "North Carolina" 1941, Ship "Wisconsin" "Iowa" 1944, Ship "Yamato" "Yamato" 1941, Ship "Yamashiro" "Yamato" 1947, Ship "South Dakota" "North Carolina" 1941, Ship "Bismarck" "North Carolina" 1911, Ship "Duke of York" "Renown" 1916, Ship "Fuso" "Iowa" 1940, Ship "Hood" "Iowa" 1942, Ship "Rodney" "Yamato" 1915, Ship "Yanashiro" "Yamato" 1918, Ship "Schamhorst" "North Carolina" 1917, Ship "Prince of Wales" "North Carolina" 1937, Ship "King George V" "Iowa" 1942, Ship "West Virginia" "Iowa" 1942 ]

database :: Database
database = (outcomes, battles, ships)

getSunk :: Database -> [(Name, [Name])]
getSunk (o, b, s) = map(\(Battle nB dB) -> (nB, (map (\(Outcome x y z) -> x)$ filter(\(Outcome x y z) -> nB == y) sunksShip))) b
 where sunksShip = filter(\(Outcome nSH nB r) -> r == "sunk" ) o

--inBattleAfterDamaged :: Database -> [Name]
--inBattleAfterDamaged (o, b, s) = filter (\(Outcome nSH nB r)  -> ) damagedShip
-- where 
--     damagedShip = filter (\(Outcome nSH nB r) -> r == "damaged" ) o
--     participatedAfterInjury = map () damagedShip

--task02
data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show)

t1 = Node 1 (Node (-1) (Node 2 Nil Nil) (Node 2 (Node 0 Nil Nil) Nil)) (Node (-1) Nil Nil)

t2 = Node 1 (Node 2 (Node 1 Nil Nil) (Node 1 (Node 10 Nil Nil) Nil)) (Node 3 Nil Nil)

getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value _ _ ) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1) 

grandchildrenIncreased :: (Eq a, Ord a) => BTree a -> Bool
grandchildrenIncreased (Node value left right)
 | (getLevel left 1 == [] && getLevel right 1 == []) = True
 | (getLevel left 1 == [] && (all (\ x -> x > value ) $ getLevel right 1)) = grandchildrenIncreased right
 | ((all (\ x -> x > value ) $ getLevel left 1) && getLevel right 1 == []) = grandchildrenIncreased left
 | ((all (\ x -> x > value ) $ getLevel left 1) &&  (all (\ x -> x > value ) $ getLevel right 1)) = grandchildrenIncreased left && grandchildrenIncreased right
 | otherwise = False
 



