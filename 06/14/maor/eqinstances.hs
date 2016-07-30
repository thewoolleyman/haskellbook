module EqInstances where
import Data.List (sort)
data TisAnInteger = 
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn i1) (TisAn i2) = i1 == i2

data TwoIntegers = 
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two i1 i2) (Two i1' i2') = i1 == i1' && i2 == i2'

data StringOrInt = 
    TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt i1) (TisAnInt i1') = i1 == i1'
    (==) (TisAString s1) (TisAString s1') = s1 == s1'
    (==) _ _ = False

data Pair a = 
    Pair a a
    deriving Show

instance Eq a => Eq (Pair a) where
    (==) (Pair a1 a2)  (Pair a1' a2') = a1 == a1' && a2 == a2'

data Tuple a b =
    Tuple a b
    deriving Show

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a1 b1) (Tuple a1' b1') = a1 == a1' && b1 == b1'

data Which a =
    ThisOne a
    | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

data EitherOr a b = 
    Hello a
    | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) _ _ = False

-- Multi Choice: 1c, 2b, 3a, 4b, 5a

-- TypeCheck: 
-- 1
-- No because Person doesnt derive show
data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2
-- No because Mood is not deriving of Eq
data Mood = Blah
    | Woot deriving Show

instance Eq Mood where
    (==) Blah Blah = True
    (==) Woot Woot = True
    (==) _ _ = False

settleDown x = if x == Woot then Blah else x 
-- 3
-- a. Woot | Blah 
-- b. It will fail because 9 is not a proper value constructor for Mood or otherwise Mood is not an instance of Num
-- c. It will fail because we only defined Eq not Ord on Mood

type Subject = String
type Verb = String
type Object = String

data Sentence = 
    Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "dogs" "drool" 
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = 
    Rocks String deriving (Eq, Show)

data Yeah = 
    Yeah Bool deriving (Eq, Show)

data Papu = 
    Papu Rocks Yeah
    deriving (Eq, Show)

phew = Papu (Rocks "chases") (Yeah True) -- will not typecheck because it needs the other arguments to be functions
truth = Papu (Rocks "chkosdk") (Yeah True) -- will typecheck

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p' -- should compile because we derive Eq

--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p' -- will not compile because we did not derive Ord

i :: Num a => a
i = 1

f :: Float
f = 1.0

f2 :: Fractional a => a
f2 = 1.0

f3 :: RealFrac a => a
f3 = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x 

{-
jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: Ord a => [a] -> [a] 
mySort = sort

signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

tst :: Eq b => a -> b
tst a  = True
-}
