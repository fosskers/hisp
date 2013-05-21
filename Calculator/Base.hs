module Calculator.Base
    ( Value(..)
    , Exp(..)
    , e ) where

import Control.Arrow (first,second)
import Control.Monad (foldM)

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


e :: Exp -> Either String Value
e (Val a)  = return a

e (Add [_]) = tooFew Add
e (Add ns)  = fold (+) 0 ns

e (Mul [_]) = tooFew Mul
e (Mul ns)  = fold (*) 1 ns

e (Sub [_])    = tooFew Sub
e (Sub (n:ns)) = e n >>= \n' -> fold (-) n' ns

--e (Div (n:ns)) = e n >>= \n' -> fold (/) n' ns

e (Pow [_])    = tooFew Pow
e (Pow (n:ns)) = e n >>= \n' -> fold (^) n' ns

e (Fac [n]) = e n >>= \n' -> return (product [1 .. n'])
e (Fac [])  = tooFew Fac
e (Fac _)   = tooMany Fac

fold :: (Value -> Value -> Value) -> Value -> [Exp] -> Either String Value
fold f = foldM (\acc n' -> (f acc) `fmap` e n')

symbol :: Exp -> String
symbol (Add _) = "+"
symbol (Mul _) = "*"
symbol (Sub _) = "-"
symbol (Pow _) = "^"
symbol (Fac _) = "!"

tooMany :: (a -> Exp) -> Either String b
tooMany f = Left $ "Too many args from " ++ symbol (f undefined)

tooFew :: (a -> Exp) -> Either String b
tooFew  f = Left $ "Too few args from " ++ symbol (f undefined)
