import Data.List

data Person = Person Bool  deriving (Show)
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


data Mood = Blah
            | Woot deriving (Show,Eq)

settleDown x = if x == Woot
               then Blah
               else x


type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

phew = Papu (Rocks "chases") (Yeah True)

truth = Papu (Rocks "chomskydoz")
              (Yeah True)  

-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

-- i :: Num a => a
-- i :: a
-- i = 1

-- f :: Float
-- f :: Num a => a
-- f :: Fractional a => a
f :: RealFrac a => a
f = 1.0

-- freud :: a -> a
freud :: Ord a => a -> a
freud x = x

-- freud' :: a -> a
freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int
-- sigmund :: a -> a
sigmund x = myX

sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a
sigmund' x = myX


-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)


-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)


mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b= (aToB a) == b


arith :: Num b =>  (a -> b)
                   -> Integer
                   -> a
                   -> b
arith aToB int a  = (aToB a) + (fromInteger int)