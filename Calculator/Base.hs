module Calculator.Base where

data Exp a = Val a
           | Add [Exp a]
           | Sub [Exp a]
           | Mul [Exp a]
           | Div [Exp a]
             deriving (Eq,Show)

e :: Fractional a => Exp a -> a
e (Val a)  = a
e (Add ns) = foldr ((+) . e) 0 ns
e (Mul ns) = foldr ((*) . e) 1 ns
e (Sub (n:ns)) = foldl (\acc n' -> acc - e n') (e n) ns
e (Div (n:ns)) = foldl (\acc n' -> acc / e n') (e n) ns
