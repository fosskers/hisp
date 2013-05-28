module REPL.Builtins where

import Data.Map (Map, fromList)

import REPL.Eval
import REPL.Types
import REPL.Utils (tau)

---

builtins :: Map String Function
builtins = fromList $ map (\f -> (funcName f, f))
           [ Function "+" (AtLeast 2) (foldE1 (+)) 
           , Function "*" (AtLeast 2) (foldE1 (*))
           , Function "-" (AtLeast 2) (foldE1 (-))
           , Function "/" (AtLeast 2) (foldE1 (/))
           , Function "^" (AtLeast 2) (foldE1 (^))
           , Function "!" (Exactly 1) (\[n] -> e n >>= \n' -> return (product [1 .. n']))
           , Function "mod" (AtLeast 2) (foldE1 mod)
           , Function "div" (AtLeast 2) (foldE1 div)
           , Function "sin" (Exactly 1) (one sin)
           , Function "cos" (Exactly 1) (one cos)
           , Function "tan" (Exactly 1) (one tan)
           , Function "log" (Exactly 1) (one log)
           , Function "exp" (Exactly 1) (one exp)
           , Function "sqrt" (Exactly 1) (one sqrt)
           , Function "tau" None (none tau)
           , Function "pi" None (none pi)
           , Function "x" None (none 0) ]

voidFun :: String -> Function
voidFun s = Function "void" (AtLeast 0)
            (const $ Left $ "{{ " ++ s ++ " }} is not a valid symbol.")
