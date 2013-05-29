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
