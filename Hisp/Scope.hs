{-# LANGUAGE FlexibleContexts #-}

module Hisp.Scope where

import Control.Monad.State.Lazy
import Data.Map.Lazy (insert,empty,foldlWithKey)

import Hisp.Utils (fork)
import Hisp.Types

---

newGlobal :: Function -> [Scope] -> [Scope]
newGlobal f@(Function n h _ _ _) ss = b ++ [insert (n,h) f g] ++ l
    where (b,g,l) = fork (length ss - 2) ss

newLambda :: Function -> [Scope] -> [Scope]
newLambda f@(Function n h _ _ _) ss = init ss ++ [insert (n,h) f $ last ss]

global :: MonadState [a] m => m a
global = head `liftM` get

-- | The Scope better not be empty!
popScope :: MonadState [a] m => m ()
popScope = modify tail

searchByName :: String -> Scope -> Maybe Function
searchByName n s = searchScope sf s
    where sf acc (n',_) f = if n == n' then f : acc else acc

searchByHash :: Hash -> Scope -> Maybe Function
searchByHash h s = searchScope sf s
    where sf acc (_,h') f = if h == h' then f : acc else acc

-- BUG? Is this returning the first match, or the last match?
searchScope :: ScopeFold -> Scope -> Maybe Function
searchScope sf s = case foldlWithKey sf [] s of
                     []    -> Nothing
                     (f:_) -> Just f

hashFromName :: String -> Scope -> Maybe Int
hashFromName n s = case searchByName n s of
                     Nothing -> Nothing
                     Just f  -> Just $ funcHash f
