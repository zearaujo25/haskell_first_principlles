data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nextNat) = 1 + (natToInteger nextNat)

integerToNat :: Integer -> Maybe Nat
integerToNat n 
            | n<0 = Nothing
            | otherwise = Just (integerToNatChecked n)

integerToNatChecked :: Integer -> Nat
integerToNatChecked n 
            | n == 0 =  Zero
            | otherwise = Succ (integerToNatChecked (n-1))