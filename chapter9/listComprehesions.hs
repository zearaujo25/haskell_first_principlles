mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]let blah = enumFromTo 'a' 'z'
myTuple = [(x,y) | x <- mySqr,y <- myCube, x<50 && y<50]
-- (print.show.lenght)$myTuple