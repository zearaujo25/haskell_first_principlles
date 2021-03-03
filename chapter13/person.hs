import Data.Char (toLower,isNumber) 
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson ::  Name
            -> Age
            -> Either PersonInvalid Person


mkPerson name age
    | name /= "" && age > 0 =
        Right $ Person name age

    | name == "" = Left NameEmpty

    | not (age > 0) = Left AgeTooLow

    | otherwise =
        Left $ PersonInvalidUnknown $
            "Name was: " ++ show name ++
            " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do 
    putStrLn "Type the persons name"
    name <- getLine
    putStrLn "Type the persons ager"
    stringAge <- getLine 
    if (not.(all isNumber)$stringAge) then
        putStrLn "Age must be a numberr"
    else do 
            let result = mkPerson name (read stringAge :: Integer)
            case (result) of 
                (Right person)  ->  putStrLn$ "Yay! Successfully got a person: " ++ (show person) 
                (Left err)      ->  putStrLn$ "Error: " ++ (show err) 

