awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

pseudoLenght :: [a] -> Int
pseudoLenght list = length list

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x<0
           then negate x
          else
           x
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f first_tuple second_tuple = ((snd first_tuple,snd second_tuple),(fst first_tuple, fst second_tuple))


x = (+)
func :: [a] -> Int 
func xs = w `x` 1
   where w = length xs 

x_id x = x

f_tuple :: (a,a) -> a
f_tuple (a,b) = a