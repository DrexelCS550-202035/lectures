data Exp = Val Int
         | Add Exp Exp

eval :: Exp -> Int
eval (Val i)     = i
eval (Add e1 e2) = eval e1 + eval e2
