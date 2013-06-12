module Hisp.Builtins where

import Control.Applicative ((<$>))
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
  , Function "="    10 Nothing (Exactly 2 []) (\es -> fromBool <$> two num (==) es)
  , Function "!"    11 Nothing (Exactly 1 []) (\es -> fromNum <$> one num (product . enumFromTo 1) es)
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
  , Function "x"    26 Nothing noArgs (none $ fromNum 0) ]

ifBlock :: [Exp] -> Evaluate Exp
ifBlock (p:a:b:_) = e p >>= is bool >>= \p' -> if p' then e a else e b
ifBlock _ = failure "Too many arguments to `if` block."

condBlock :: [(Exp,Exp)] -> Evaluate Exp
condBlock ((p,a):es) = e p >>= is bool >>= \p' -> if p' then e a else condBlock es
condBlock [] = failure "Non-terminating `cond` block given."
