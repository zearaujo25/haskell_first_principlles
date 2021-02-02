newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show
data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show
data Farmer = Farmer Name Acres FarmerType deriving Show

--unpacking values from product type
isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True 
isDairyFarmer _ = False


data FarmerRec = FarmerRec { name :: Name
                            , acres :: Acres
                            , farmerType :: FarmerType }
                            deriving Show

-- unpacking record type 
isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
    case farmerType farmer of
        DairyFarmer -> True
        _        -> False