eftBool :: Bool -> Bool -> [Bool]
eftBool from to = 
    case compare from to of
        GT -> []
        EQ -> to:[]
        LT -> from:(eftBool (succ from) to)


eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to = 
    case compare from to of
        GT -> []
        EQ -> to:[]
        LT -> from:(eftOrd (succ from) to)


eftInt :: Int -> Int -> [Int]
eftInt from to = 
    case compare from to of
        GT -> []
        EQ -> to:[]
        LT -> from:(eftInt (succ from) to)

eftChar :: Char -> Char -> [Char]
eftChar from to = 
    case compare from to of
        GT -> []
        EQ -> to:[]
        LT -> from:(eftChar (succ from) to)