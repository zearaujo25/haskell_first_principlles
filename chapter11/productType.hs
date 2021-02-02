data Person = Person { name :: String, age :: Int } deriving (Eq, Show)

-- sample data
jm = Person "julie" 108
ca = Person "chris" 16




-- data Fiction = Fiction deriving (Show,Eq)
-- data Nonfiction = Nonfiction deriving (Show,Eq)

-- data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show


type AuthorName = String
-- Not normal form
-- data Author = Author (AuthorName, BookType)

--Normal form
data Author =  Fiction AuthorName  |   Nonfiction AuthorName deriving (Eq, Show)



-- data FlowerType =  Gardenia
--     |  Daisy
--     |  Rose
--     |  Lilac
--     deriving Show

type Gardener = String

-- not normal form
-- data Garden = Garden Gardener FlowerType deriving Show

-- normal form
data Garden =   Gardenia Gardener  | 
                Daisy Gardener |
                Rose Gardener  |
                Lilac Gardener  