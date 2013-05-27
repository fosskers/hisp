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
           , Function "sin" (Exactly 1) (one sin)
           , Function "cos" (Exactly 1) (one cos)
           , Function "log" (Exactly 1) (one log)
           , Function "exp" (Exactly 1) (one exp)
           , Function "tau" None (const $ return tau)
           , Function "pi" None (const $ return pi) ]

voidFun :: Function
voidFun = Function "void" (AtLeast 0) (const $ Left "Non-existant function called.")
