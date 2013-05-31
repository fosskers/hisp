module Hisp.Eval
    ( e
    , one
    , none
    , foldE
    , foldE1 ) where

import Prelude hiding (lookup)
import Control.Monad.State.Lazy
import Data.Map.Lazy (lookup, fromList, empty)
import Control.Applicative ((<*))

import Hisp.Types

---

-- | The Evaluation Function
e :: Exp -> Evaluate Value
e (Val a)     = return a
e (Call f es) = get >>= function f >>= \f' ->
                argCheck f' es >> local f' es >> apply f' es <* popScope

argCheck :: Function -> [Exp] -> Evaluate a
argCheck f es | numOkay (funcArgs f) (length es) = return undefined
              | otherwise = failure $ badArgs f es

function :: String -> [Scope] -> Evaluate Function
function f [] = failure $ "{{ " ++ f ++ " }} is not a valid symbol in any scope."
function f (s:ss) = case lookup f s of
                      Nothing -> function f ss
                      Just f' -> return f'

-- Does the `(AtLeast 0)` there not matter?
-- | Add a local scope based on a function's namespace.
local :: Function -> [Exp] -> Evaluate ()
local (Function _ (AtLeast _) _) _     = modify (empty :)
local (Function _ (Exactly _ ss) _) es = modify (ns :)
    where ns = fromList $ zipWith toF ss es
          toF s (Val a)      = (s, Function s noArgs (none a))
          toF s (Call f es') = (s, Function s (AtLeast 0)
                                   (\es'' -> e $ Call f (es' ++ es'')))

numOkay :: Args -> Int -> Bool
numOkay (Exactly i _) n = n == i
numOkay (AtLeast i)   n = n >= i

badArgs :: Function -> [a] -> String
badArgs f es = "Wrong number of args given to: {{ " ++ funcName f ++ " }}" ++
               "\nNeeds " ++ necArgs (funcArgs f) ++ " but was given " ++
               show (length es) ++ "."

-- | For functions that take exactly one argument.
one :: (Value -> a) -> [Exp] -> Evaluate a
one f [n] = f `fmap` e n
one _ _   = failure "Single arg function applied to multiple arguments."

-- | For functions that take no arguments.
none :: Monad m => a -> b -> m a
none = const . return

foldE :: (Value -> Value -> Value) -> Value -> [Exp] -> Evaluate Value
foldE f = foldM (\acc n' -> f acc `fmap` e n')

foldE1 :: (Value -> Value -> Value) -> [Exp] -> Evaluate Value
foldE1 _ []     = failure "No Expressions available to fold."  -- Needed?
foldE1 f (n:ns) = e n >>= \n' -> foldE f n' ns
