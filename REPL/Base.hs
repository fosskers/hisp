module REPL.Base
    ( initialState
    , inject
    , e ) where

import REPL.Types
import Control.Monad.State.Lazy

---

initialState :: REPLState
initialState = [I 0, I 0, I 0]

-- | Inject a new Value into the REPLState.
-- Signature will always stay the same while the implementation changes :)
inject :: Monad m => Value -> StateT REPLState m ()
inject v = get >>= \rs -> unless (v `elem` rs) (put $ take 3 (v : rs))

-- | The Evaluation Function
e :: Exp -> Either String Value
e (Val a)  = return a

e (Add [_]) = tooFew Add
e (Add ns)  = foldE (+) 0 ns

e (Mul [_]) = tooFew Mul
e (Mul ns)  = foldE (*) 1 ns

e (Sub [_])    = tooFew Sub
e (Sub (n:ns)) = e n >>= \n' -> foldE (-) n' ns

e (Div (n:ns)) = e n >>= \n' -> foldE (/) n' ns

e (Pow [_])    = tooFew Pow
e (Pow (n:ns)) = e n >>= \n' -> foldE (^) n' ns

e (Fac [n]) = e n >>= \n' -> return (product [1 .. n'])
e (Fac [])  = tooFew Fac
e (Fac _)   = tooMany Fac

---

foldE :: (Value -> Value -> Value) -> Value -> [Exp] -> Either String Value
foldE f = foldM (\acc n' -> (f acc) `fmap` e n')

replError :: (a -> Exp) -> String -> Either String b
replError f msg = Left $ msg ++ symbol (f undefined)

tooMany :: (a -> Exp) -> Either String b
tooMany f = replError f "Too many args from "

tooFew :: (a -> Exp) -> Either String b
tooFew  f = replError f "Too few args from "

symbol :: Exp -> String
symbol (Add _) = "+"
symbol (Mul _) = "*"
symbol (Sub _) = "-"
symbol (Div _) = "/"
symbol (Pow _) = "^"
symbol (Fac _) = "!"
