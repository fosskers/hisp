module Calculator.Base where

import Control.Arrow (first,second)

---

data Exp = Val Value
         | Add [Exp]
         | Sub [Exp]
         | Mul [Exp]
         | Div [Exp]
         | Pow [Exp]
         | Fac [Exp]
           deriving (Eq,Show)

-- Ord might need a specific declaration.
-- Or else all I's might come before any D's regardless of number.
data Value = I Integer
           | D Double deriving (Eq,Ord)

asI :: Value -> Value
asI = I . toInteger . fromEnum

instance Show Value where
    show (I i) = show i
    show (D d) = show d

instance Enum Value where
    toEnum = I . toInteger
    fromEnum (I i) = fromEnum i
    fromEnum (D d) = fromEnum d

instance Real Value where
    toRational (I i) = toRational i
    toRational (D d) = toRational d

instance Integral Value where
    toInteger (I i) = i
    toInteger v     = toInteger $ asI v
    quotRem (I i1) (I i2) = first I . second I $ quotRem i1 i2
    quotRem x y = quotRem (asI x) (asI y)

instance Num Value where
    I i + I j = I $ i + j
    I i + D d = D $ fromIntegral i + d
    D d + I i = D $ d + fromIntegral i
    D d + D f = D $ d + f

    I i * I j = I $ i * j
    I i * D d = D $ fromIntegral i * d
    D d * I i = D $ d * fromIntegral i
    D d * D f = D $ d * f

    I i - I j = I $ i - j
    I i - D d = D $ fromIntegral i - d
    D d - I i = D $ d - fromIntegral i
    D d - D f = D $ d - f

    abs (I i) = I $ abs i
    abs (D d) = D $ abs d

    signum (I i) = I $ signum i
    signum (D d) = D $ signum d

    fromInteger = I


e :: Exp -> Value
e (Val a)  = a
e (Add ns) = foldr ((+) . e) 0 ns
e (Mul ns) = foldr ((*) . e) 1 ns
e (Sub (n:ns)) = foldl (\acc n' -> acc - e n') (e n) ns
--e (Div (n:ns)) = foldl (\acc n' -> acc / e n') (e n) ns
e (Pow (n:ns)) = foldl (\acc n' -> acc ^ e n') (e n) ns
e (Fac [n]) = product [1 .. (e n)]
