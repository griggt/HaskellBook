module Vehicles where

data Price = Price Integer 
    deriving (Eq, Show)

data Size = Size Integer
    deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
    deriving (Eq, Show)

data Airline = PapuaAir | CatapultsR'Us | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

---

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuaAir (Size 130)

-- exercise #2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False
    
isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- exercise #3
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
-- note that this is a partial function and does not handle Plane inputs!

-- exercise #5
-- see changes to code above
