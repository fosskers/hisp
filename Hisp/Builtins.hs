module Hisp.Builtins where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (filterM)
import Data.Map (fromList)

import Hisp.Eval
import Hisp.Types
import Hisp.Utils (tau,twinZip)

---

builtins :: Scope
builtins = fromList . map (\f -> ((funcName f, funcHash f), f)) . concat $
           [ controlFunctions, mathFunctions, listFunctions, otherFunctions ]

-- `let` will go in here too.
controlFunctions :: [Function]
controlFunctions =
  [ Function "if"     1 Nothing (Exactly 3 []) ifBlock
  , Function "cond"   2 Nothing (AtLeast 2)    (condBlock . twinZip)
  , Function "else"   3 Nothing noArgs (none $ fromBool True)
  , Function "apply"  4 Nothing (AtLeast 2)   (\(x:es) -> apply' x es) ]

mathFunctions :: [Function]
mathFunctions =
  [ Function "+"    101 Nothing (AtLeast 2)    (evalNum foldE1 (+))
  , Function "*"    102 Nothing (AtLeast 2)    (evalNum foldE1 (*))
  , Function "-"    103 Nothing (AtLeast 2)    (evalNum foldE1 (-))
  , Function "/"    104 Nothing (AtLeast 2)    (evalNum foldE1 (/))
  , Function "^"    105 Nothing (AtLeast 2)    (evalNum foldE1 (^))
  , Function "<"    106 Nothing (Exactly 2 []) (\es -> fromBool <$> two num (<) es)
  , Function ">"    107 Nothing (Exactly 2 []) (\es -> fromBool <$> two num (>) es)
  , Function ">="   108 Nothing (Exactly 2 []) (\es -> fromBool <$> two num (>=) es)
  , Function "<="   109 Nothing (Exactly 2 []) (\es -> fromBool <$> two num (<=) es)
  , Function "="    110 Nothing (Exactly 2 [])
    (\(x:y:_) -> (\z -> fromBool . (z ==)) <$> e x <*> e y)
  , Function "!"    111 Nothing (Exactly 1 []) (evalNum one (product . enumFromTo 1))
  , Function "mod"  112 Nothing (AtLeast 2)    (evalNum foldE1 mod)
  , Function "div"  113 Nothing (AtLeast 2)    (evalNum foldE1 div)
  , Function "abs"  114 Nothing (Exactly 1 []) (evalNum one abs)
  , Function "sin"  115 Nothing (Exactly 1 []) (evalNum one sin)
  , Function "cos"  116 Nothing (Exactly 1 []) (evalNum one cos)
  , Function "tan"  117 Nothing (Exactly 1 []) (evalNum one tan)
  , Function "log"  118 Nothing (Exactly 1 []) (evalNum one log)
  , Function "exp"  120 Nothing (Exactly 1 []) (evalNum one exp)
  , Function "sqrt" 121 Nothing (Exactly 1 []) (evalNum one sqrt)
  , Function "tau"  122 Nothing noArgs (none $ fromNum tau)
  , Function "pi"   123 Nothing noArgs (none $ fromNum pi)
  , Function "x"    124 Nothing noArgs (none $ fromNum 0) ]
--  , Function "range" 125 Nothing (Exactly 2 []) (\(x:y:_) -> range x y) ]

listFunctions :: [Function]
listFunctions =
  [ Function ":"    201 Nothing (Exactly 2 []) (\(x:es:_) -> is lst es >> cons x es)
  , Function "head" 202 Nothing (Exactly 1 []) (\(x:_) -> is lst x >> car x)
  , Function "tail" 203 Nothing (Exactly 1 []) (\(x:_) -> is lst x >> cdr x)
  , Function "concat" 204 Nothing (AtLeast 2)  (\es -> List <$> foldE1 lst (++) es)
  , Function "len"  205 Nothing (Exactly 1 []) (\(x:_) -> is lst x >> len x)
  , Function "rangeH" 206 Nothing (Exactly 2 []) (\(x:y:_) -> rangeH x y)
  , Function "mapH" 207 Nothing (Exactly 2 [])
    (\(f:es:_) -> is sym f >> is lst es >> mapH f es)
  , Function "filterH" 208 Nothing (Exactly 2 [])
    (\(f:es:_) -> is sym f >> is lst es >> filterH f es) ]

otherFunctions :: [Function]
otherFunctions = []
--  [ Function "show" 901 Nothing (Exactly 1 []) (\(x:_) -> return (Val . S $ show x)) ]

ifBlock :: [Exp] -> Evaluate Exp
ifBlock (p:a:b:_) = e p >>= is bool >>= \p' -> if p' then e a else e b
ifBlock _ = failure "Too many arguments to `if` block."

condBlock :: [(Exp,Exp)] -> Evaluate Exp
condBlock ((p,a):es) = e p >>= is bool >>= \p' -> if p' then e a else condBlock es
condBlock [] = failure "Non-terminating `cond` block given."

type Evaluator a b = (Exp -> Either String Number) -> a -> b -> Evaluate Number

evalNum :: Evaluator a b -> a -> b -> Evaluate Exp
evalNum f g es = fromNum <$> f num g es

-----------------
-- List functions
-----------------
cons :: Exp -> Exp -> Evaluate Exp
cons x l@(List ((Symbol _ _):_)) = e x >>= \x' -> evalList (return . List . (x' :)) l
cons x (List es) = e x >>= \x' -> return $ List $ x' : es
cons _ _         = failure "Second argument was not a List."

car :: Exp -> Evaluate Exp
car (List []) = failure "Empty list."
car l         = evalList (return . head) l

cdr :: Exp -> Evaluate Exp
cdr (List (_:es)) = return $ List es
cdr _ = failure "Empty list."

len :: Exp -> Evaluate Exp
len l = evalList (return . fromNum . I . toInteger . length) l

evalList :: ([Exp] -> Evaluate a) -> Exp -> Evaluate a
evalList f l = e l >>= \l' ->
  case l' of
    List l'' -> f l''
    _        -> failure "Second argument did not evaluate to a List."

-- This is much faster than the Hisp version.
rangeH :: Exp -> Exp -> Evaluate Exp
rangeH x y = do
  x' <- e x >>= is num
  y' <- e y >>= is num
  return . List . map (Val . N) $ [x' .. y']

mapH :: Exp -> Exp -> Evaluate Exp
mapH s l = evalList (\es -> List `fmap` mapM (\ex -> e $ List [s,ex]) es) l

filterH :: Exp -> Exp -> Evaluate Exp
filterH s l = evalList (\es -> List `fmap` filterM f es) l
    where f ex = do
            result <- e $ List [s,ex]
            case bool result of
              Right True  -> return True
              Right False -> return False
              Left _ -> failure "Function application did not yield a Boolean."

apply' :: Exp -> [Exp] -> Evaluate Exp
apply' f es = (List . (f :)) `fmap` mapM e es >>= e
