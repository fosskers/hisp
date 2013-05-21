module Calculator.Base where

{-}
data Exp a = Val a
           | Add [Exp a]
           | Sub [Exp a]
           | Mul [Exp a]
           | Div [Exp a]
           | Pow [Exp a]
           | Fac [Exp a]
             deriving (Eq,Show)
-}

data Exp = Val Value
         | Add [Exp]
         | Sub [Exp]
         | Mul [Exp]
         | Div [Exp]
         | Pow [Exp]
         | Fac [Exp]
           deriving (Eq,Show)

data Value = RI Int 
           | RD Double deriving (Eq)

instance Show Value where
    show (RI i) = show i
    show (RD d) = show d

instance Num Value where
    RI i + RI j = RI $ i + j
    RI i + RD d = RD $ fromIntegral i + d
    RD d + RI i = RD $ d + fromIntegral i
    RD d + RD f = RD $ d + f

    RI i * RI j = RI $ i * j
    RI i * RD d = RD $ fromIntegral i * d
    RD d * RI i = RD $ d * fromIntegral i
    RD d * RD f = RD $ d * f

    RI i - RI j = RI $ i - j
    RI i - RD d = RD $ fromIntegral i - d
    RD d - RI i = RD $ d - fromIntegral i
    RD d - RD f = RD $ d - f

    abs (RI i) = RI $ abs i
    abs (RD d) = RD $ abs d

    signum (RI i) = RI $ signum i
    signum (RD d) = RD $ signum d

    fromInteger = RI . fromInteger    

e :: Exp -> Value
e (Val a)  = a
e (Add ns) = foldr ((+) . e) 0 ns
e (Mul ns) = foldr ((*) . e) 1 ns
e (Sub (n:ns)) = foldl (\acc n' -> acc - e n') (e n) ns
--e (Div (n:ns)) = foldl (\acc n' -> acc / e n') (e n) ns
--e (Pow (n:ns)) = foldl (\acc n' -> acc ** e n') (e n) ns
--e (Fac [n]) = product [1 .. round (e n)]
