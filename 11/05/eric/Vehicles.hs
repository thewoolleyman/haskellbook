module Vehicles where

data Price =
  Price Integer deriving (Eq, Show)

data Size =
  Size Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsRUs
             | TakeYourChancesUnited
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir (Size 100)

isCar :: Vehicle -> Bool
isCar c = case c of
            Car _ _ -> True
            _ -> False

isPlane :: Vehicle -> Bool
isPlane p = case p of
              Plane _ _ -> True
              _ -> False

getManu :: Vehicle -> Manufacturer
getManu v = case v of
              Car manu _ -> manu
              _ -> undefined

