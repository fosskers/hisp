module Hisp.Eval where

import Prelude hiding (lookup)
import Control.Monad.State.Lazy
import Data.Map.Lazy (lookup)

import Hisp.Types

---

-- | The Evaluation Function
e :: Exp -> Evaluate Value
e (Val a)        = return a
e (FunCall f es) = get >>= getFun f >>= \f' -> localScope f' es >> apply f' es

argCheck :: Function -> [Exp] -> Evaluate a
argCheck f es | numOkay (funcArgs f) (length es) = return undefined
              | otherwise = failure . badArgs . funcName $ f

getFun :: String -> [Scope] -> Evaluate Function
getFun f [] = failure $ "{{ " ++ f ++ " }} is not a valid symbol in any scope."
getFun f (s:ss) = case lookup f s of
                    Nothing -> getFun f ss
                    Just f' -> return f'

localScope :: Function -> [Exp] -> Evaluate ()
localScope = undefined

numOkay :: Args -> Int -> Bool
numOkay (Exactly i _) n = n == i
numOkay (AtLeast i)   n = n >= i

badArgs :: String -> String
badArgs s = "Wrong number of args given to: {{ " ++ s ++ " }}"

-- | For functions that take exactly one argument.
one :: (Value -> a) -> [Exp] -> Evaluate a
one f [n] = f `fmap` e n
one _ _   = failure "You should never see this."

-- | For functions that take no arguments.
none :: Monad m => a -> b -> m a
none = const . return

foldE :: (Value -> Value -> Value) -> Value -> [Exp] -> Evaluate Value
foldE f = foldM (\acc n' -> f acc `fmap` e n')

foldE1 :: (Value -> Value -> Value) -> [Exp] -> Evaluate Value
foldE1 _ []     = failure "No Expressions available to fold."  -- Needed?
foldE1 f (n:ns) = e n >>= \n' -> foldE f n' ns
