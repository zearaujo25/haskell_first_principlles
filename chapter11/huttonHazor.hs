data Expr = Lit Integer | Add Expr Expr
eval :: Expr -> Integer
eval (Lit val) = val
eval (Add ex1 ex2) = (eval ex1) + (eval ex2)

printExpr :: Expr -> String
printExpr (Lit val) = show val
printExpr (Add ex1 ex2) = (printExpr ex1) ++ "+" ++ (printExpr ex2)