{-# LANGUAGE FlexibleContexts #-}

module Hisp.Base where

import Control.Monad.State.Lazy
import Data.Map.Lazy (insert,empty)

import Hisp.Utils (fork)
import Hisp.Eval  (none)
import Hisp.Builtins
import Hisp.Types

---

-- | The bottom Scope is for lambdas. The next is for global functions.
initialState :: [Scope]
initialState = [builtins,empty]

-- | Inject a new Value into Scope.
-- Signature will always stay the same while the implementation changes :)
inject :: Monad m => Value -> StateT [Scope] m ()
inject v = get >>= \ss -> put (newGlobal (newX v) ss)

newX :: Value -> Function
newX x = Function "x" 26 Nothing (Exactly 0 []) (none x)

newGlobal :: Function -> [Scope] -> [Scope]
newGlobal f@(Function n h _ _ _) ss = b ++ [insert (n,h) f g] ++ l
    where (b,g,l) = fork (length ss - 2) ss

newLambda :: Function -> [Scope] -> [Scope]
newLambda f@(Function n h _ _ _) ss = init ss ++ [insert (n,h) f $ last ss]
