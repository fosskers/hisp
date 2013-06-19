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

newtype Evaluate a = E { runE :: ErrorT EvalError (StateT [Scope] IO) a }
  deriving ( Monad, MonadError EvalError, MonadState [Scope]
           , MonadIO, Functor, Applicative)

{-}
newtype Evaluate a = E { runE :: ErrorT EvalError (State [Scope]) a }
  deriving ( Monad, MonadError EvalError, MonadState [Scope]
           , Functor, Applicative)
-}

data EvalError = M { errMsg :: String } deriving (Eq,Show)

instance Error EvalError where
    noMsg  = strMsg "No error message given."
    strMsg = M

eval :: Evaluate a -> [Scope] -> IO (Either EvalError a, [Scope])
eval a s = runStateT (runErrorT $ runE a) s

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

---

data Args = Exactly Int [Exp] | AtLeast Int deriving (Eq,Show)

data Function = Function { funcName :: String
                         , funcHash :: Hash
                         , funcBody :: Maybe Exp
                         , funcArgs :: Args
                         , apply    :: [Exp] -> Evaluate Exp }

instance Show Function where
    show f = funcName f ++ " (" ++ as' ++ ")"
        where as  = argString $ funcArgs f
              as' = if null as then "" else as

argString :: Args -> String
argString (Exactly i _) = intersperse ' ' (take i ['a'..'z'])
argString (AtLeast i)   = intersperse ' ' (take i ['a'..'z']) ++ ".."

noArgs :: Args
noArgs = Exactly 0 []

necArgs :: Args -> String
necArgs (Exactly i _) = "exactly " ++ show i
necArgs (AtLeast i)   = "at least " ++ show i

----------------
-- S-EXPRESSIONS
----------------
data Exp = Comment
         | Val Value
         | Symbol String Hash
         | Require { reqPath :: FilePath }
         | List { expList :: [Exp] }
           deriving (Eq,Ord)

instance Show Exp where
    show Comment      = "Comment"
    show (Val v)      = show v
    show (Symbol n _) = n -- ++ " @ " ++ show h
    show (Require fp) = "require -> " ++ fp
    show l@(List es) | null es    = "()"
                     | isString l = foldr (\(Val (C c)) s -> c : s) "" es
                     | otherwise  = "(" ++ unwords (map show es) ++ ")"

typeName :: Exp -> String
typeName Comment      = "Comment"
typeName (Val (N _))  = "Number"
typeName (Val (B _))  = "Boolean"
typeName (Val (C _))  = "Char"
typeName (Symbol _ _) = "Symbol"
typeName (Require _)  = "Require"
typeName (List _)     = "List"

lst :: Exp -> Either String [Exp]
lst (List es) = Right es
lst ex        = badType "List" ex

sym :: Exp -> Either String Exp
sym s@(Symbol _ _) = Right s
sym ex             = badType "Symbol" ex

isSymbol :: Exp -> Bool
isSymbol (Symbol _ _) = True
isSymbol _            = False

isString :: Exp -> Bool
isString (List es) = and $ map isChar es
isString _         = False

isChar :: Exp -> Bool
isChar (Val (C _)) = True
isChar _           = False

isList :: Exp -> Bool
isList (List _) = True
isList _        = False

isRequire :: Exp -> Bool
isRequire (Require _) = True
isRequire _           = False

------------------
-- HISP DATA TYPES
------------------
data Value = N Number
           | B Bool
           | C Char deriving (Eq,Ord)

instance Show Value where
    show (N n) = show n
    show (B b) = show b
    show (C c) = show c

instance Hashable Value where
    hashWithSalt s (N n) = hashWithSalt s n
    hashWithSalt s (B b) = hashWithSalt s b
    hashWithSalt s (C c) = hashWithSalt s c

is :: (Exp -> Either String a) -> Exp -> Evaluate a
is t e = case t e of
           Right x -> return x
           Left s  -> failure $ "Expression of wrong type given. " ++ s

badType :: String -> Exp -> Either String a
badType s ex = Left $ "Wanted `" ++ s ++ "`, given `" ++ typeName ex ++ "`."

-- | A Numeric type of pure evil. Converts between Integers and Doubles
-- where necessary. This allows it to be `Integral` and `Floating` at the same
-- time.
data Number = I Integer | D Double deriving (Eq)

instance Show Number where
    show (I i) = show i
    show (D d) = show d

-- | Important for typechecking.
num :: Exp -> Either String Number
num (Val (N n)) = Right n
num ex          = badType "Number" ex

bool :: Exp -> Either String Bool
bool (Val (B b)) = Right b
bool ex          = badType "Boolean" ex

-- The opposites.
fromNum :: Number -> Exp
fromNum = Val . N

fromBool :: Bool -> Exp
fromBool = Val . B

--fromStr :: String -> Exp
--fromStr = Val. S

asI :: Number -> Number
asI i@(I _) = i
asI (D d)   = I . toInteger . fromEnum $ d

asD :: Number -> Number
asD d@(D _) = d
asD (I i)   = D $ fromInteger i

instance Hashable Number where
    hashWithSalt s (I i) = hashWithSalt s i
    hashWithSalt s (D d) = hashWithSalt s d

instance Ord Number where
    compare (I i) (I j) = compare i j
    compare (I i) (D d) = compare (fromIntegral i) d
    compare (D d) (I i) = compare d (fromIntegral i)
    compare (D d) (D h) = compare d h

instance Enum Number where
    toEnum = I . toInteger
    fromEnum (I i) = fromEnum i
    fromEnum (D d) = fromEnum d

instance Fractional Number where
    fromRational = D . fromRational
    (D d1) / (D d2) = D $ d1 / d2
    x / y = asD x / asD y

instance Real Number where
    toRational (I i) = toRational i
    toRational (D d) = toRational d

instance Integral Number where
    toInteger (I i) = i
    toInteger v     = toInteger $ asI v
    quotRem (I i1) (I i2) = first I . second I $ quotRem i1 i2
    quotRem x y = quotRem (asI x) (asI y)

-- Yes, boilerplate!
instance Floating Number where
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

instance Num Number where
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
