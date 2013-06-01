module Hisp.Builtins where

import Data.Map (Map, fromList)

import Hisp.Eval
import Hisp.Types
import Hisp.Utils (tau, toN)

---

builtins :: Map String Function
builtins = fromList $ map (\f -> (funcName f, f))
           [ Function "+"    (AtLeast 2)    (foldE1 (+))
           , Function "*"    (AtLeast 2)    (foldE1 (*))
           , Function "-"    (AtLeast 2)    (foldE1 (-))
           , Function "/"    (AtLeast 2)    (foldE1 (/))
           , Function "^"    (AtLeast 2)    (foldE1 (^))
           , Function "="    (Exactly 2 []) (two equal)
           , Function "!"    (Exactly 1 []) (one (product . toN))
           , Function "if"   (Exactly 3 []) (three ifBlock)
           , Function "mod"  (AtLeast 2)    (foldE1 mod)
           , Function "div"  (AtLeast 2)    (foldE1 div)
           , Function "abs"  (Exactly 1 []) (one abs)
           , Function "sin"  (Exactly 1 []) (one sin)
           , Function "cos"  (Exactly 1 []) (one cos)
           , Function "tan"  (Exactly 1 []) (one tan)
           , Function "log"  (Exactly 1 []) (one log)
           , Function "exp"  (Exactly 1 []) (one exp)
           , Function "sqrt" (Exactly 1 []) (one sqrt)
           , Function "tau"  noArgs (none tau)
           , Function "pi"   noArgs (none pi)
           , Function "x"    noArgs (none 0) ]

equal :: Value -> Value -> Value
equal x y = B $ x == y

ifBlock :: Value -> Value -> Value -> Value
ifBlock p a b = if isTrue p then a else b
