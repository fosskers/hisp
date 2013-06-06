{-# LANGUAGE TupleSections #-}

-- Simple s-expression repl.

import Hisp.Eval
import Hisp.Base
import Hisp.Utils
import Hisp.Types
import Hisp.Parser

import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe

import qualified Data.Map as M

---

main :: IO ()
main = do
  putStrLn "Welcome to the Hisp REPL. Type `h` for help."
  void $ runStateT run initialState

run :: StateT [Scope] IO ()
run = forever $ do
  putStr' "> "
  input <- liftIO $ runMaybeT (getLine' >>= nest [])
  case input of
    Nothing  -> putStrLn' ">>> Too many right parentheses."
    Just []  -> return ()
    Just "h" -> help
    Just cs  -> get >>= \ss -> do
      output <- case eval (parse cs >>= e) ss of
                  (Left err, _)  -> return $ errMsg err
                  (Right v, ss') -> put ss' >> inject v >> return (show v)
      putStrLn' $ ">>> " ++ output

help :: StateT [Scope] IO ()
help = global >>= \bs -> liftIO $ do
  let names = M.foldr (\f acc -> funcName f : acc) [] bs
  mapM_ putStrLn [ "Hisp REPL Help"
                 , "Available functions:"
                 , "  [ " ++ unwords names ++ " ]"
                 , "1. `x` stores what was calculated last."
                 , "2. You can define your own functions with `define`:"
                 , "     (define foo 5)"
                 , "     (define bar (* foo 2))"
                 , "     (define baz [a b] (+ a b))"
                 , "3. You can pass functions and also partially apply them:"
                 , "     (define $ [f a] (f a))"
                 , "     ($ sin 1)"
                 , "     ($ (+ 1) 1)"
                 , "     ($ (lambda [a] (+ 1 a)) 1)"
                 , "4. Feel free to write expressions over multiple lines." ]

parse :: String -> Evaluate Exp
parse s = get >>= \rs -> case parseExp rs s of
                           Left err -> failure $ "Syntax Error\n" ++ show err
                           Right (ex,rs') -> put rs' >> return ex

-- Only notes the locations of left parens.
nest :: [Int] -> String -> MaybeT IO String
nest ps line =
  case parens ps line of
    Nothing  -> MaybeT $ return Nothing
    Just []  -> return line
    Just ps' -> do
      let pad = replicate (head ps') ' '
      putStr' $ ".  " ++ pad
      input <- (('\n' : pad) ++) `fmap` getLine'
      (line ++) `fmap` nest ps' input

-- Change to record location of first char of first arg!!
-- | Can fail if there are too many right parens.
parens :: [Int] -> String -> Maybe [Int]
parens ps line = pop $ fold (1,ps,0) line
    where fold (_,ls,rs) []       = (ls,rs)
          fold (n,ls,rs) ('(':xs) = fold (n + 1, indent n xs : ls, rs) xs
          fold (n,ls,rs) (')':xs) = fold (n + 1, ls, rs + 1) xs
          fold (n,ls,rs) (_:xs)   = fold (n + 1, ls, rs) xs

-- | The position an argument should be indented to on the next line.
indent :: Int -> String -> Int
indent p []      = p
indent p (' ':_) = p
indent p ('(':_) = p - 1
indent p (_:ns)  = indent (p + 1) ns

-- | Pops a left paren off the stack for each right paren found.
pop :: ([Int],Int) -> Maybe [Int]
pop (ls,rs) | rs > length ls = Nothing
            | otherwise      = Just $ drop rs ls
