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

import Data.Map.Lazy       (fromList, empty, lookup, insert)
import Data.Hashable       (hash)

import Hisp.Types
import Hisp.Scope

---

-- | The Evaluation Function
e :: Exp -> Evaluate Exp
e v@(Val _)      = return v
e s@(Symbol _ _) = return s
e (List (x:es))  = e x >>= \x' ->
  case x' of
    Symbol n h -> do
      f   <- get >>= function n h
      f'  <- argCheck f es >> local f es >> bindParamCalls f
--      depth <- length `fmap` get
--      liftIO $ putStrLn $ replicate depth '-' ++ " " ++ n ++ " @ " ++ show h
      result <- apply f' es
      when (null es) $ share result (n,h)  -- Share the result to other scopes!!
      popScope >> return result
    _ -> (List . (x' :)) `fmap` mapM e es
e l@(List []) = return l

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
local (Function _ _ _ (Exactly _ ah) _) es = do
  depth <- length `fmap` get
  let ns = fromList $ zipWith toF ah es
      toF (Symbol a _) v@(Val v') =
          let h = hash v' in
          ((a,h), Function a h Nothing noArgs (none v))
      toF (Symbol a _) (List es') =
          let h = hash $ show es' ++ show depth in
          ((a,h), Function a h Nothing (AtLeast 0)
           (\es'' -> e $ List (es' ++ es'')))
      toF (Symbol a _) s@(Symbol _ h') =
          ((a,h'), Function a h' Nothing (AtLeast 0) (none s))
--                   (\es' -> e $ List (s : es')))  -- Infinite recursion...
  modify (ns :)

-- | Needs to be called after `local`.
-- No body means it's a builtin function.
-- Having a body means it was parsed, and is thus a written Hisp function.
-- Thus, it might have instances of parameter calls.
bindParamCalls :: Function -> Evaluate Function
bindParamCalls f@(Function _ _ Nothing _ _) = return f
bindParamCalls (Function n h (Just b) as _) = head `fmap` get >>= \ls -> do
  let b' = connect ls b
  return $ Function n h (Just b') as (const $ e b')

-- BUG: This doesn't seem to be working.
-- June 20: Really?
connect :: Scope -> Exp -> Exp
connect _ v@(Val _) = v
connect s (List es) = List $ map (connect s) es
connect s (Symbol n h) = Symbol n h'
    where h' = case hashFromName n s of Nothing -> h; Just h'' -> h''

share :: Exp -> Address -> Evaluate ()
share ex a = modify $ share' ex a

share' :: Exp -> Address -> [Scope] -> [Scope]
share' _ _ [ ]           = []
share' ex a@(n,h) (s:ss) =
  case lookup a s of
    Nothing -> s : share' ex a ss
    Just _  -> insert a new s : ss
        where new = Function n h Nothing (AtLeast 0) (none ex)

numOkay :: Args -> Int -> Bool
numOkay (Exactly i _) n = n == i
numOkay (AtLeast i)   n = n >= i

badArgs :: Function -> [a] -> String
badArgs f es = "Wrong number of args given to: {{ " ++ funcName f ++ " }}" ++
               "\nNeeds " ++ necArgs (funcArgs f) ++ " but was given " ++
               show (length es) ++ "."

-- | For functions that take exactly one argument.
one :: (Exp -> Either String a) -> (a -> b) -> [Exp] -> Evaluate b
one t f [n] = fmap f $ e n >>= is t
one _ _ _   = failure "Single arg function applied to multiple arguments."

-- | For functions that take exactly two arguments.
two :: (Exp -> Either String a) -> (a -> a -> b) -> [Exp] -> Evaluate b
two t f (x:y:_) = liftM2 f (e x >>= is t) (e y >>= is t)
two _ _ _       = failure "Two arg function applied to less than two arguments."

three :: (Exp -> Either String a) -> (a -> a -> a -> b) -> [Exp] -> Evaluate b
three t f (x:y:z:_) = liftM3 f (e x >>= is t) (e y >>= is t) (e z >>= is t)
three _ _ _ = failure "Three arg function applied to less than three arguments."

-- | For functions that take no arguments.
none :: Monad m => a -> b -> m a
none = const . return

foldE :: (Exp -> Either String a) -> (a -> a -> a) -> a -> [Exp] -> Evaluate a
foldE t f = foldM (\acc x -> f acc `fmap` (e x >>= is t))

foldE1 :: (Exp -> Either String a) -> (a -> a -> a) -> [Exp] -> Evaluate a
foldE1 _ _ []     = failure "No Expressions available to fold."  -- Needed?
foldE1 t f (x:xs) = e x >>= is t >>= \x' -> foldE t f x' xs
