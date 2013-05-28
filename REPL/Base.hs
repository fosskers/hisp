module REPL.Base where

import Control.Monad.State.Lazy
import Data.Map.Lazy (insert)

import REPL.Eval (none)
import REPL.Types
import REPL.Builtins

---

initialState :: REPLState
initialState = builtins

-- | Inject a new Value into the REPLState registers.
-- Signature will always stay the same while the implementation changes :)
inject :: Monad m => Value -> StateT REPLState m ()
inject v = modify $ insert "x" (newX v)

newX :: Value -> Function
newX x = Function "x" None (none x)