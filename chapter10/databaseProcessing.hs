import Data.Time
data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase = [   DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
                , DbNumber 9001
                , DbNumber 9003
                , DbString "Hello, world!"
                , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))]

getDate :: DatabaseItem-> [UTCTime] -> [UTCTime]
getDate (DbDate x) acc  = acc ++ [x]
getDate _  acc = acc

getNumber :: DatabaseItem-> [Integer] -> [Integer]
getNumber (DbNumber x) acc  = acc ++ [x]
getNumber _  acc = acc





filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate dataBase = foldr getDate [] dataBase   


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber dataBase = foldr getNumber [] dataBase 


getMinimumDate :: DatabaseItem-> UTCTime -> UTCTime
getMinimumDate (DbDate x) acc  = min x acc
getMinimumDate _  acc = acc

mostRecent :: [DatabaseItem]-> UTCTime 
mostRecent dataBase = foldr getMinimumDate (UTCTime (fromGregorian 9999 12 31) (secondsToDiffTime 34123)) dataBase

sumDbAcc :: DatabaseItem -> Integer -> Integer
sumDbAcc (DbNumber x) acc  = acc + x
sumDbAcc _  acc = acc

sumDb :: [DatabaseItem] -> Integer
sumDb dataBase = foldr sumDbAcc 0 dataBase

avgDbAcc :: DatabaseItem -> (Integer,Integer) -> (Integer,Integer)
avgDbAcc (DbNumber x) (acc,nItems) = (acc+x,nItems+1)
avgDbAcc _  acc = acc

avgDb :: [DatabaseItem] -> Double
avgDb = (\(acc,nItems) -> (fromInteger acc)/(fromInteger nItems)).(foldr avgDbAcc (0,0))
