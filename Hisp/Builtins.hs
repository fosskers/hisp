module Hisp.Builtins where

import Data.Map (Map, fromList)

import Hisp.Eval
import Hisp.Types
import Hisp.Utils (tau,twinZip)

---

builtins :: Map String Function
builtins = fromList $ map (\f -> (funcName f, f))
           [ Function "+"    (AtLeast 2)    (foldE1 (+))
           , Function "*"    (AtLeast 2)    (foldE1 (*))
           , Function "-"    (AtLeast 2)    (foldE1 (-))
           , Function "/"    (AtLeast 2)    (foldE1 (/))
           , Function "^"    (AtLeast 2)    (foldE1 (^))
           , Function "<"    (Exactly 2 []) (two ((B .) . (<)))
           , Function ">"    (Exactly 2 []) (two ((B .) . (>)))
           , Function ">="   (Exactly 2 []) (two ((B .) . (>=)))
           , Function "<="   (Exactly 2 []) (two ((B .) . (<=)))
           , Function "="    (Exactly 2 []) (two ((B .) . (==)))
           , Function "!"    (Exactly 1 []) (one (product . enumFromTo 1))
           , Function "if"   (Exactly 3 []) ifBlock
           , Function "mod"  (AtLeast 2)    (foldE1 mod)
           , Function "div"  (AtLeast 2)    (foldE1 div)
           , Function "abs"  (Exactly 1 []) (one abs)
           , Function "sin"  (Exactly 1 []) (one sin)
           , Function "cos"  (Exactly 1 []) (one cos)
           , Function "tan"  (Exactly 1 []) (one tan)
           , Function "log"  (Exactly 1 []) (one log)
           , Function "exp"  (Exactly 1 []) (one exp)
           , Function "sqrt" (Exactly 1 []) (one sqrt)
           , Function "cond" (AtLeast 2)    (condBlock . twinZip)
           , Function "else" noArgs (none $ B True)
           , Function "tau"  noArgs (none tau)
           , Function "pi"   noArgs (none pi)
           , Function "x"    noArgs (none 0) ]

ifBlock :: [Exp] -> Evaluate Value
ifBlock (p:a:b:_) = e p >>= \p' -> if isTrue p' then e a else e b
ifBlock _ = failure "Too many arguments to `if` block."

condBlock :: [(Exp,Exp)] -> Evaluate Value
condBlock ((p,a):es) = e p >>= \p' -> if isTrue p' then e a else condBlock es
condBlock [] = failure "Non-terminating `cond` block given."
