import Data.Char
import Data.List
data DaPhone = DaPhone [PhoneButton] deriving Show
-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int
type PhoneButton = String

button1 = "1"
button2 =  "2abc"
button3 =  "3def"
button4 =  "4ghi"
button5 =  "5jkl"
button6 =  "6mno"
button7 =  "7pqrs"
button8 =  "8tuv"
button9 =  "9wxyz"
buttonStar =  "*^"
button0 =  "0+ "
buttonHash =  "#.,"

phone = DaPhone [button0,
                button1,
                button2,
                button3,
                button4,
                button5,
                button6,
                button7,
                button8,
                button9,
                buttonStar,
                buttonHash]
cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone text= concat.(map (reverseTaps phone))$text


reverseTaps :: DaPhone -> Char -> [(Digit, Presses)] 
reverseTaps phone char 
    | (isUpper char) = [('*',1)] ++ ((reverseTaps phone).toLower$char)
    | otherwise = [ getTaps.head.(filter filterButtons).(phoneChecker char)$phone]

phoneChecker :: Char->DaPhone->[(Digit,Maybe Int)]
phoneChecker char (DaPhone phone) = map (buttonChecker char) phone


buttonChecker :: Char->PhoneButton->(Digit,Maybe Int)
buttonChecker char button = (button!!0,(elemIndex char button))

 
filterButtons :: (Digit,Maybe Int) -> Bool
filterButtons (_,Nothing) = False
filterButtons _ = True

getTaps :: (Digit,Maybe Int) -> (Digit,Presses)
getTaps (x, Just taps) = (x,taps+1)

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps convoTaps = foldr (\(_,buttonTaps) -> \totalTaps -> totalTaps+buttonTaps) 0 convoTaps

mostPopularLetter :: String -> Char
mostPopularLetter  =  fst. letterCount

letterCount :: String -> (Char,Int)
letterCount = (maximumBy (\t1 -> \t2 -> compare (snd t1) (snd t2))).(map (\letterGroup -> ((head letterGroup),(length letterGroup)))).group.sort.(filter isAlpha).(map toLower)


coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter.concat

-- coolestWord :: [String] -> String
coolestWord = fst.(maximumBy (\t1 -> \t2 -> compare (snd t1) (snd t2))).(map (\letterGroup -> ((head letterGroup),(length letterGroup)))).group.sort.(map lowerWord).concat.(map words)

lowerWord :: String -> String
lowerWord = map toLower

convo :: [String]
convo = [   "Wanna play 20 questions",
            "Ya",
            "U 1st haha",
            "Lol ok. Have u ever tasted alcohol",
            "Lol ya",
            "Wow ur cool haha. Ur turn",
            "Ok. Do u think I am pretty Lol",
            "Lol ya",
            "Just making sure rofl ur turn"]