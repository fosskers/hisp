-- Simple s-expression repl.

import REPL.Base
import REPL.Types
import REPL.Parser

import System.IO (stdout, hFlush)
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe

---

main :: IO ()
main = void $ runStateT run initialState

run :: StateT REPLState IO ()
run = forever $ do
  putStr' "> "
  input <- liftIO $ runMaybeT (getLine' >>= nest [])
  case input of
    Nothing -> putStrLn' ">>> Too many right parentheses."
    Just [] -> return ()
    Just cs -> get >>= \rs -> do
                  output <- case rpn rs cs of
                              Left err -> return err
                              Right v  -> inject v >> return (show v)
                  putStrLn' $ ">>> " ++ output

rpn :: REPLState -> String -> Either String Value
rpn rs s = case parseExp rs s of
          Left err   -> Left $ "Syntax Error\n" ++ show err
          Right sexp -> e sexp

-- Only notes the locations of left parens.
nest :: [Int] -> String -> MaybeT IO String
nest ps line =
  case parens ps line of
    Nothing  -> MaybeT $ return Nothing
    Just []  -> return line
    Just ps' -> do
      let pos = head ps' + 2
          pad = replicate pos ' '
      putStr' $ ".  " ++ pad
      input <- (('\n' : pad) ++) `fmap` getLine'
      (line ++) `fmap` nest ps' input

-- | Can fail if there are too many right parens.
parens :: [Int] -> String -> Maybe [Int]
parens ps line = pop . dropFst . foldl fold (0,ps,0) $ line
    where fold (n,ls,rs) '(' = (n + 1, n : ls, rs)
          fold (n,ls,rs) ')' = (n + 1, ls, rs + 1)
          fold (n,ls,rs) _   = (n + 1, ls, rs)

-- | Pops a left paren off the stack for each right paren found.
pop :: ([Int],Int) -> Maybe [Int]
pop (ls,rs) | rs > length ls = Nothing
            | otherwise      = Just $ drop rs ls

dropFst :: (a,b,c) -> (b,c)
dropFst (_,b,c) = (b,c)

putStr' :: MonadIO m => String -> m ()
putStr' s = liftIO (putStr s >> hFlush stdout)

putStrLn' :: MonadIO m => String -> m ()
putStrLn' s = putStr' $ s ++ "\n"

getLine' :: MaybeT IO String
getLine' = liftIO getLine
