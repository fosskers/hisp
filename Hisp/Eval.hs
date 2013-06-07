module Hisp.Eval
    ( e
    , one
    , two
    , three
    , none
    , foldE
    , foldE1 ) where

import Prelude hiding (lookup)

import Control.Monad.State.Lazy

import Data.Map.Lazy       (fromList, empty)
import Control.Applicative ((<*),(<$>),(<*>))
import Data.Hashable       (hash)

import Hisp.Types

---

-- | The Evaluation Function
e :: Exp -> Evaluate Value
e (Val a)       = return a
e (Call f h es) = get >>= function f h >>= \f' -> argCheck f' es >> local f' es >>
                  bindParamCalls f' >>= \f'' -> apply f'' es <* popScope

argCheck :: Function -> [Exp] -> Evaluate a
argCheck f es | numOkay (funcArgs f) (length es) = return undefined
              | otherwise = failure $ badArgs f es

function :: String -> Hash -> [Scope] -> Evaluate Function
function _ _ [] = failure "Eval.function: You should never see this."
function n h ss =
    case functionByHash h ss of
      Just f  -> return f
      Nothing -> case functionByName n ss of
                   Just f  -> return f
                   Nothing -> failure $ "{{ " ++ n ++ " }} is not in any scope."

functionByHash :: Hash -> [Scope] -> Maybe Function
functionByHash _ []     = Nothing
functionByHash 0 _      = Nothing
functionByHash h (s:ss) = case searchByHash h s of
                            Nothing -> functionByHash h ss
                            Just f  -> return f

functionByName :: String -> [Scope] -> Maybe Function
functionByName _ []     = Nothing
functionByName n (s:ss) = case searchByName n s of
                            Nothing -> functionByName n ss
                            Just f  -> return f

-- Does the `(AtLeast 0)` there not matter?
-- | Add a local scope based on a function's namespace.
-- Binds arguments to parameter names.
local :: Function -> [Exp] -> Evaluate ()
local (Function _ _ _ (AtLeast _) _) _     = modify (empty :)
local (Function _ _ _ (Exactly _ ah) _) es = modify (ns :)
    where ns = fromList $ zipWith toF ah es
          toF (a,_) (Val v) = ((a,h), Function a h Nothing noArgs (none v))
              where h = hash v
          toF (a,_) (Call f h' es') = ((a,h), Function a h Nothing (AtLeast 0)
                                       (\es'' -> e $ Call f h' (es' ++ es'')))
              where h = hash $ show es'  -- Must be very unique.

-- | Needs to be called after `local`.
-- No body means it's a builtin function.
-- Having a body means it was parsed, and is thus a written Hisp function.
-- Thus, it might have instances of parameter calls.
bindParamCalls :: Function -> Evaluate Function
bindParamCalls f@(Function _ _ Nothing _ _) = return f
bindParamCalls (Function n h (Just b) as _) = head `fmap` get >>= \ls -> do
  let b' = connect ls b
  return $ Function n h (Just b') as (const $ e b')

connect :: Scope -> Exp -> Exp
connect _ v@(Val _) = v
connect s (Call f h es) = Call f h' es'
    where h'  = case hashFromName f s of Nothing -> h; Just h'' -> h''
          es' = map (connect s) es

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

-- | For functions that take exactly two arguments.
two :: (Value -> Value -> a) -> [Exp] -> Evaluate a
two f (x:y:_) = f <$> e x <*> e y
two _ _       = failure "Two arg function applied to less than two arguments."

three :: (Value -> Value -> Value -> a) -> [Exp] -> Evaluate a
three f (x:y:z:_) = f <$> e x <*> e y <*> e z
three _ _ = failure "Three arg function applied to less than three arguments."

-- | For functions that take no arguments.
none :: Monad m => a -> b -> m a
none = const . return

foldE :: (Value -> Value -> Value) -> Value -> [Exp] -> Evaluate Value
foldE f = foldM (\acc n' -> f acc `fmap` e n')

foldE1 :: (Value -> Value -> Value) -> [Exp] -> Evaluate Value
foldE1 _ []     = failure "No Expressions available to fold."  -- Needed?
foldE1 f (n:ns) = e n >>= \n' -> foldE f n' ns
