data Quantum = Yes
                | No
                | Both
                deriving (Eq, Show)

-- 3 + 3, sum type all possible values
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes
quantSum2 :: Either Quantum Quantum
quantSum2 = Right No
quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both
quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes
-- You can fill in the next two.
quantSum5 :: Either Quantum Quantum
quantSum5 = Left No
quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

-- 3 * 3, product type all possble values
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)
quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)
quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)
quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)
quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)
quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)
quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)
-- You can determine the final two.
quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)
quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)
-- function type, 2³
convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = True
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False