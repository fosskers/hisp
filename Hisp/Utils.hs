module Hisp.Utils where

import Control.Monad.Trans
import System.IO (stdout, hFlush)

---

putStr' :: MonadIO m => String -> m ()
putStr' s = liftIO (putStr s >> hFlush stdout)

putStrLn' :: MonadIO m => String -> m ()
putStrLn' s = putStr' $ s ++ "\n"

getLine' :: MonadIO m => m String
getLine' = liftIO getLine

tau :: Floating a => a
tau = 6.283185307179586

fork :: Int -> [a] -> ([a],a,[a])
fork n xs = (b, head a, tail a)
    where (b,a) = splitAt n xs

twinZip :: [a] -> [(a,a)]
twinZip []       = []
twinZip [_]      = []
twinZip (x:y:zs) = (x,y) : twinZip zs
