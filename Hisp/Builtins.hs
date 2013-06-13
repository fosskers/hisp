module Hisp.Builtins where

import Control.Applicative ((<$>), (<*>))
import Data.Map (fromList)

import Hisp.Eval
import Hisp.Types
import Hisp.Utils (tau,twinZip)

---

builtins :: Scope
builtins =
  fromList $ map (\f -> ((funcName f, funcHash f), f))
  [ Function "+"     1 Nothing (AtLeast 2)    (\es -> fromNum <$> foldE1 num (+) es)
  , Function "*"     2 Nothing (AtLeast 2)    (\es -> fromNum <$> foldE1 num (*) es)
  , Function "-"     3 Nothing (AtLeast 2)    (\es -> fromNum <$> foldE1 num (-) es)
  , Function "/"     4 Nothing (AtLeast 2)    (\es -> fromNum <$> foldE1 num (/) es)
  , Function "^"     5 Nothing (AtLeast 2)    (\es -> fromNum <$> foldE1 num (^) es)
  , Function "<"     6 Nothing (Exactly 2 []) (\es -> fromBool <$> two num (<) es)
  , Function ">"     7 Nothing (Exactly 2 []) (\es -> fromBool <$> two num (>) es)
  , Function ">="    8 Nothing (Exactly 2 []) (\es -> fromBool <$> two num (>=) es)
  , Function "<="    9 Nothing (Exactly 2 []) (\es -> fromBool <$> two num (<=) es)
  , Function "="    10 Nothing (Exactly 2 [])
    (\(x:y:_) -> (\z -> fromBool . (z ==)) <$> e x <*> e y)
  , Function "!"    11 Nothing (Exactly 1 [])
    (\es -> fromNum <$> one num (product . enumFromTo 1) es)
  , Function "if"   12 Nothing (Exactly 3 []) ifBlock
  , Function "mod"  13 Nothing (AtLeast 2)    (\es -> fromNum <$> foldE1 num mod es)
  , Function "div"  14 Nothing (AtLeast 2)    (\es -> fromNum <$> foldE1 num div es)
  , Function "abs"  15 Nothing (Exactly 1 []) (\es -> fromNum <$> one num abs es)
  , Function "sin"  16 Nothing (Exactly 1 []) (\es -> fromNum <$> one num sin es)
  , Function "cos"  17 Nothing (Exactly 1 []) (\es -> fromNum <$> one num cos es)
  , Function "tan"  18 Nothing (Exactly 1 []) (\es -> fromNum <$> one num tan es)
  , Function "log"  19 Nothing (Exactly 1 []) (\es -> fromNum <$> one num log es)
  , Function "exp"  20 Nothing (Exactly 1 []) (\es -> fromNum <$> one num exp es)
  , Function "sqrt" 21 Nothing (Exactly 1 []) (\es -> fromNum <$> one num sqrt es)
  , Function "cond" 22 Nothing (AtLeast 2)    (condBlock . twinZip)
  , Function "else" 23 Nothing noArgs (none $ fromBool True)
  , Function "tau"  24 Nothing noArgs (none $ fromNum tau)
  , Function "pi"   25 Nothing noArgs (none $ fromNum pi)
  , Function "x"    26 Nothing noArgs (none $ fromNum 0)
  , Function ":"    27 Nothing (Exactly 2 []) (\(x:es:_) -> cons x es)
  , Function "head" 28 Nothing (Exactly 1 []) (\(x:_) -> car x)
  , Function "tail" 29 Nothing (Exactly 1 []) (\(x:_) -> cdr x)
  , Function "range" 30 Nothing (Exactly 2 []) (\(x:y:_) -> range x y)
  , Function "apply" 31 Nothing (AtLeast 2)   (\(x:es) -> apply' x es) ]

ifBlock :: [Exp] -> Evaluate Exp
ifBlock (p:a:b:_) = e p >>= is bool >>= \p' -> if p' then e a else e b
ifBlock _ = failure "Too many arguments to `if` block."

condBlock :: [(Exp,Exp)] -> Evaluate Exp
condBlock ((p,a):es) = e p >>= is bool >>= \p' -> if p' then e a else condBlock es
condBlock [] = failure "Non-terminating `cond` block given."

-----------------
-- List functions
-----------------
cons :: Exp -> Exp -> Evaluate Exp
cons x (List es) = return $ List $ x : es
{-}
  l' <- e l  -- I'm meh about this. Breaks lazy evaluation, doesn't it?
  case l' of
    List l'' -> return $ List $ x : l''
    _         -> failure "Second argument did not evaluate to a List."
-}
cons _ _ = failure "Second argument was not a List."

car :: Exp -> Evaluate Exp
car (List (x:_)) = return x
car _ = failure "Empty list."

cdr :: Exp -> Evaluate Exp
cdr (List (_:es)) = return $ List es
cdr _ = failure "Empty list."

range :: Exp -> Exp -> Evaluate Exp
range x y = do
  x' <- e x >>= is num
  y' <- e y >>= is num
  return . List . map (Val . N) $ [x' .. y']

apply' :: Exp -> [Exp] -> Evaluate Exp
apply' f es = (List . (f :)) `fmap` mapM e es >>= e
