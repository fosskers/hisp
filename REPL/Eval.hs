module REPL.Eval where

import Control.Monad (foldM)

import REPL.Types

---

-- | The Evaluation Function
e :: Exp -> Either String Value
e (Val a)        = return a
e (FunCall f es) = argCheck f es >> apply f es

argCheck :: Function -> [Exp] -> Either String a
argCheck f es | numOkay (argNum f) (length es) = return undefined
              | otherwise = Left . badArgs . funcName $ f

numOkay :: ArgNum -> Int -> Bool
numOkay None n        = n == 0
numOkay (Exactly i) n = n == i
numOkay (AtLeast i) n = n >= i

badArgs :: String -> String
badArgs s = "Wrong number of args given to: {{ " ++ s ++ " }}"

-- | For functions that take exactly one argument.
one :: (Value -> a) -> [Exp] -> Either String a
one f [n] = f `fmap` e n
one _ _   = error "You should never see this."

foldE :: (Value -> Value -> Value) -> Value -> [Exp] -> Either String Value
foldE f = foldM (\acc n' -> f acc `fmap` e n')

foldE1 :: (Value -> Value -> Value) -> [Exp] -> Either String Value
foldE1 _ []     = Left "No Expressions available to fold."  -- Needed?
foldE1 f (n:ns) = e n >>= \n' -> foldE f n' ns
