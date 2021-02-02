data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a , psecond :: b } deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool
--later exercise
type Awesome = Bool

type PoundsOfWool = Int 
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)
-- Alternately
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

bess' = (CowInfo "Bess" 4)
bess = First bess' :: Animal'

e' = Second (SheepInfo "Elmer" 5 5)
elmer = Second e' :: Animal'


elmo' = Second (SheepInfo "Elmo" 5 5)

--mistake
-- elmo = First elmo' :: Animal'


-- ****building nullary****
trivialValue :: GuessWhat
trivialValue = Chickenbutt


-- ****building unnary****
-- note:
-- MkId :: a -> Id a
idInt :: Id Integer
idInt = MkId 10


-- ****building a product****
person :: Product Name Awesome
person = Product "Simon" True


-- ****building  sum ****
-- data Twitter = Twitter deriving (Eq, Show)
-- data AskFm =  AskFm deriving (Eq, Show)

-- this will force Twitter be always First and  AskFm be always Second
-- socialNetwork :: Sum Twitter AskFm
-- socialNetwork = First Twitter

-- This wont matter the order
data SocialNetwork = Twitter | AskFm deriving (Eq, Show)


-- this opens for mistake because both are strings and compiler have no wway of knowing which is which, because 
-- in he end both are strings
type Twitter = String
type AskFm = String
twitter :: Sum Twitter AskFm
twitter = First "Twitter"
askfm :: Sum Twitter AskFm
askfm = First "AskFm"


-- ****Record and product types****
-- building a record and a product type are identical. Records are just syntax to create field references

--building like product
-- myRecord :: RecordProduct Integer Float
-- myRecord = Recor dProduct 42 0.00001

--building using fields
myRecord :: RecordProduct Integer Float
myRecord = RecordProduct { pfirst = 42 , psecond = 0.00001 }

-- useful with domain names
data OperatingSystem = 
    GnuPlusLinux  
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
                , lang :: ProgLang }
    deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }
-- We can reorder stuff
-- when we use record syntax
feelingWizardly :: Programmer
feelingWizardly =
    Programmer { lang = Agda
                , os = GnuPlusLinux }


-- ***EXERCISE***
allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]


allProgrammers :: [Programmer]
allProgrammers = [ Programmer { lang = x, os = y } | x <- allLanguages,y <- allOperatingSystems]