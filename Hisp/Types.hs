{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hisp.Types
    ( Value(..)
    , Exp(..)
    , Scope
    , Args(..)
    , Function(..)
    , Evaluate
    , global
    , eval
    , failure) where

import Control.Monad.State.Lazy
import Control.Monad.Error
import Control.Arrow (first,second)
import Data.List     (intersperse)
import Data.Map      (Map)

import Hisp.Utils (tau)

---

newtype Evaluate a = E { runE :: ErrorT EvalError (State [Scope]) a }
  deriving ( Monad, MonadError EvalError, MonadState [Scope], Functor)

data EvalError = M String deriving (Eq,Show)

instance Error EvalError where
    noMsg  = strMsg "No error message given."
    strMsg = M

eval :: Evaluate a -> [Scope] -> Either EvalError a
eval a s = evalState (runErrorT $ runE a) s

failure :: String -> Evaluate a
failure = throwError . strMsg

---

type Scope = Map String Function

global :: Monad m => Scope -> m (Map String Function)
global = return

--local :: Monad m => Scope -> m (Map String Function)
--local = undefined

data Args = Exactly Int [String] | AtLeast Int deriving (Eq,Show)

data Function = Function { funcName :: String
                         , funcArgs :: Args
                         , apply    :: [Exp] -> Evaluate Value }

instance Show Function where
    show f = "(lambda [" ++ as ++ "] (" ++ funcName f ++ " " ++ as ++ "))"
        where as = argString $ funcArgs f

argString :: Args -> String
argString (Exactly i _) = intersperse ' ' (take i ['a'..'z'])
argString (AtLeast i)   = intersperse ' ' (take i ['a'..'z']) ++ ".."

noArgs :: Args
noArgs = Exactly 0 []

data Exp = Val Value | FunCall String [Exp] deriving (Show)

-- Ord might need a specific declaration.
-- Or else all I's might come before any D's regardless of number.
-- | A Numeric type of pure evil. Converts between Integers and Doubles
-- where necessary. This allows it to be `Integral` and `Floating` at the same
-- time.
data Value = I Integer
           | D Double deriving (Eq,Ord)

asI :: Value -> Value
asI i@(I _) = i
asI (D d)   = I . toInteger . fromEnum $ d

asD :: Value -> Value
asD d@(D _) = d
asD (I i)   = D $ fromInteger i

instance Show Value where
    show (I i) = show i
    show (D d) = show d

instance Enum Value where
    toEnum = I . toInteger
    fromEnum (I i) = fromEnum i
    fromEnum (D d) = fromEnum d

instance Fractional Value where
    fromRational = D . fromRational
    (D d1) / (D d2) = D $ d1 / d2
    x / y = asD x / asD y

instance Real Value where
    toRational (I i) = toRational i
    toRational (D d) = toRational d

instance Integral Value where
    toInteger (I i) = i
    toInteger v     = toInteger $ asI v
    quotRem (I i1) (I i2) = first I . second I $ quotRem i1 i2
    quotRem x y = quotRem (asI x) (asI y)

-- Yes, boilerplate!
instance Floating Value where
    pi = D $ tau / 2

    exp (D d) = D $ exp d
    exp x     = exp $ asD x
    log (D d) = D $ log d
    log x     = exp $ asD x
    sin (D d) = D $ sin d
    sin x     = exp $ asD x
    cos (D d) = D $ cos d
    cos x     = cos $ asD x

    sinh (D d) = D $ sinh d
    sinh x     = sinh $ asD x
    cosh (D d) = D $ cosh d
    cosh x     = cosh $ asD x
    asin (D d) = D $ asin d
    asin x     = asin $ asD x
    acos (D d) = D $ acos d
    acos x     = acos $ asD x
    atan (D d) = D $ atan d
    atan x     = atan $ asD x

    asinh (D d) = D $ asinh d
    asinh x     = asinh $ asD x
    acosh (D d) = D $ acosh d
    acosh x     = acosh $ asD x
    atanh (D d) = D $ atanh d
    atanh x     = atanh $ asD x

instance Num Value where
    I i + I j = I $ i + j
    D d + D f = D $ d + f
    x + y = asD x + asD y

    I i * I j = I $ i * j
    D d * D f = D $ d * f
    x * y = asD x * asD y

    I i - I j = I $ i - j
    D d - D f = D $ d - f
    x - y = asD x - asD y

    abs (I i) = I $ abs i
    abs (D d) = D $ abs d

    signum (I i) = I $ signum i
    signum (D d) = D $ signum d

    fromInteger = I
