module Hisp.Builtins where

import Data.Map (fromList)

import Hisp.Eval
import Hisp.Types
import Hisp.Utils (tau,twinZip)

---

builtins :: Scope
builtins = fromList $ map (\f -> ((funcName f, funcHash f), f))
           [ Function "+"     1 Nothing (AtLeast 2)    (foldE1 (+))
           , Function "*"     2 Nothing (AtLeast 2)    (foldE1 (*))
           , Function "-"     3 Nothing (AtLeast 2)    (foldE1 (-))
           , Function "/"     4 Nothing (AtLeast 2)    (foldE1 (/))
           , Function "^"     5 Nothing (AtLeast 2)    (foldE1 (^))
           , Function "<"     6 Nothing (Exactly 2 []) (two ((B .) . (<)))
           , Function ">"     7 Nothing (Exactly 2 []) (two ((B .) . (>)))
           , Function ">="    8 Nothing (Exactly 2 []) (two ((B .) . (>=)))
           , Function "<="    9 Nothing (Exactly 2 []) (two ((B .) . (<=)))
           , Function "="    10 Nothing (Exactly 2 []) (two ((B .) . (==)))
           , Function "!"    11 Nothing (Exactly 1 []) (one (product . enumFromTo 1))
           , Function "if"   12 Nothing (Exactly 3 []) ifBlock
           , Function "mod"  13 Nothing (AtLeast 2)    (foldE1 mod)
           , Function "div"  14 Nothing (AtLeast 2)    (foldE1 div)
           , Function "abs"  15 Nothing (Exactly 1 []) (one abs)
           , Function "sin"  16 Nothing (Exactly 1 []) (one sin)
           , Function "cos"  17 Nothing (Exactly 1 []) (one cos)
           , Function "tan"  18 Nothing (Exactly 1 []) (one tan)
           , Function "log"  19 Nothing (Exactly 1 []) (one log)
           , Function "exp"  20 Nothing (Exactly 1 []) (one exp)
           , Function "sqrt" 21 Nothing (Exactly 1 []) (one sqrt)
           , Function "cond" 22 Nothing (AtLeast 2)    (condBlock . twinZip)
           , Function "else" 23 Nothing noArgs (none $ B True)
           , Function "tau"  24 Nothing noArgs (none tau)
           , Function "pi"   25 Nothing noArgs (none pi)
           , Function "x"    26 Nothing noArgs (none 0) ]

ifBlock :: [Exp] -> Evaluate Value
ifBlock (p:a:b:_) = e p >>= \p' -> if isTrue p' then e a else e b
ifBlock _ = failure "Too many arguments to `if` block."

condBlock :: [(Exp,Exp)] -> Evaluate Value
condBlock ((p,a):es) = e p >>= \p' -> if isTrue p' then e a else condBlock es
condBlock [] = failure "Non-terminating `cond` block given."
