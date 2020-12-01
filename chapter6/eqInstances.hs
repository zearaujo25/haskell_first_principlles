data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where 
    (==) (TisAn int1) (TisAn int2) = int1==int2


data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where 
    (==) (Two int1a int1b) (Two int2a int2b) = int1a==int2a && int1b==int2b


data StringOrInt = TisAnInt Int | TisAString String


instance Eq StringOrInt where 
    (==) (TisAnInt int1) (TisAnInt int2) = int1 == int2
    (==) (TisAString str1) (TisAString str2) = str1 == str2


data Pair a = Pair a a

instance Eq a => Eq (Pair a) where 
    (==) (Pair a1 a2) (Pair b1 b2) = a1 == b1 && a2 == b2


data Tuple a b = Tuple a b
instance (Eq a,Eq b) => Eq (Tuple a b) where 
    (==) (Tuple a1 b1) (Tuple a2 b2) = a1 == a2 && b1 == b2

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where 
    (==) (ThisOne a1) (ThisOne a2) = a1 == a2
    (==) (ThatOne a1) (ThatOne a2) = a1 == a2

data EitherOr a b = Hello a | Goodbye b

instance (Eq a,Eq b) => Eq (EitherOr a b) where 
    (==) (Hello a1) (Hello a2) = a1 == a2
    (==) (Goodbye b1) (Goodbye b2) = b1 == b2
    (==) _ _ =  False


