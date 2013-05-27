module REPL.Base where

import Control.Monad.State.Lazy

import REPL.Types
import REPL.Builtins

---

initialState :: REPLState
initialState = ([I 0, I 0, I 0], builtins)

-- | Inject a new Value into the REPLState registers.
-- Signature will always stay the same while the implementation changes :)
inject :: Monad m => Value -> StateT REPLState m ()
inject v = get >>= \(rs,bs) -> unless (v `elem` rs) (put (take 3 (v : rs),bs))
