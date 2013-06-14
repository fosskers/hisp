{-# LANGUAGE FlexibleContexts #-}

module Hisp.Base where

import Control.Monad.State.Lazy

import Hisp.Types
import Hisp.Scope
import Hisp.Eval (none)

---

-- | Inject a new Value into Scope.
-- Signature will always stay the same while the implementation changes :)
inject :: Monad m => Exp -> StateT [Scope] m ()
inject v = get >>= \ss -> put (newGlobal (newX v) ss)

newX :: Exp -> Function
newX x = Function "x" 124 Nothing (Exactly 0 []) (none x)

symbolString :: Monad m => Exp -> m String
symbolString (Symbol s _) = return s
symbolString _            = fail "Expression given not a valid symbol."

-- There must be a better way to do this.
-- This screams fmap...
hispMap :: (Exp -> Exp) -> Exp -> Exp
hispMap f (List es) = List $ map f es
hispMap f e         = f e

hispMapM :: Monad m => (Exp -> m Exp) -> Exp -> m Exp
hispMapM f (List es) = List `liftM` mapM f es
hispMapM f e         = f e
