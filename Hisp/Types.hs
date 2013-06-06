{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}  -- For those MonadState signatures...

module Hisp.Types where

--import Prelude hiding (lookup)

import qualified Data.Map as M

import Control.Applicative (Applicative)
import Control.Arrow (first,second)
import Data.List     (intersperse)
import Control.Monad.State.Lazy
import Control.Monad.Error
import Data.Hashable

import Hisp.Utils (tau)

---

--newtype Evaluate a = E { runE :: ErrorT EvalError (State [Scope]) a }
--  deriving ( Monad, MonadError EvalError, MonadState [Scope], Functor, Applicative)

newtype Evaluate a = E { runE :: ErrorT EvalError (State [Scope]) a }
  deriving ( Monad, MonadError EvalError, MonadState [Scope]
           , Functor, Applicative)

data EvalError = M { errMsg :: String } deriving (Eq,Show)

instance Error EvalError where
    noMsg  = strMsg "No error message given."
    strMsg = M

eval :: Evaluate a -> [Scope] -> (Either EvalError a, [Scope])
eval a s = runState (runErrorT $ runE a) s

failure :: String -> Evaluate a
failure = throwError . strMsg

--------
-- SCOPE
--------
type Hash      = Int
type Address   = (String,Hash)
type Scope     = M.Map Address Function
type ScopeFold = ([Function] -> Address -> Function -> [Function])

noHash :: Hash
noHash = 0

global :: MonadState [a] m => m a
global = head `liftM` get

-- | The Scope better not be empty!
popScope :: MonadState [a] m => m ()
popScope = modify tail

searchByName :: String -> Scope -> Maybe Function
searchByName n s = searchScope sf s
    where sf acc (n',_) f = if n == n' then f : acc else acc

searchByHash :: Hash -> Scope -> Maybe Function
searchByHash h s = searchScope sf s
    where sf acc (_,h') f = if h == h' then f : acc else acc

searchScope :: ScopeFold -> Scope -> Maybe Function
searchScope sf s = case M.foldlWithKey sf [] s of
                     []    -> Nothing
                     (f:_) -> Just f

hashFromName :: String -> Scope -> Maybe Int
hashFromName n s = case searchByName n s of
                     Nothing -> Nothing
                     Just f  -> Just $ funcHash f

---

data Args = Exactly Int [(String,Int)] | AtLeast Int deriving (Eq,Show)

data Function = Function { funcName :: String
                         , funcHash :: Hash
                         , funcBody :: Maybe Exp
                         , funcArgs :: Args
                         , apply    :: [Exp] -> Evaluate Value }

instance Show Function where
    show f = "(lambda [" ++ as ++ "] (" ++ funcName f ++ as' ++ "))"
        where as  = argString $ funcArgs f
              as' = if null as then "" else " " ++ as

argString :: Args -> String
argString (Exactly i _) = intersperse ' ' (take i ['a'..'z'])
argString (AtLeast i)   = intersperse ' ' (take i ['a'..'z']) ++ ".."

noArgs :: Args
noArgs = Exactly 0 []

necArgs :: Args -> String
necArgs (Exactly i _) = "exactly " ++ show i
necArgs (AtLeast i)   = "at least " ++ show i

data Exp = Val Value | Call String Hash [Exp] deriving (Show,Eq,Ord)

-- Ord might need a specific declaration.
-- Or else all I's might come before any D's regardless of number.
-- | A Numeric type of pure evil. Converts between Integers and Doubles
-- where necessary. This allows it to be `Integral` and `Floating` at the same
-- time.
data Value = I Integer
           | D Double
           | B Bool deriving (Eq,Ord)
--           | L [Exp] deriving (Eq,Ord)

isTrue :: Value -> Bool
isTrue (B True) = True
isTrue _        = False

isFalse :: Value -> Bool
isFalse (B False) = True
isFalse _         = False

asI :: Value -> Value
asI i@(I _) = i
asI (D d)   = I . toInteger . fromEnum $ d

asD :: Value -> Value
asD d@(D _) = d
asD (I i)   = D $ fromInteger i

instance Show Value where
    show (I i) = show i
    show (D d) = show d
    show (B b) = show b
--    show (L l) = show l

instance Hashable Value where
    hashWithSalt s (I i) = hashWithSalt s i
    hashWithSalt s (D d) = hashWithSalt s d
    hashWithSalt s (B b) = hashWithSalt s b

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
