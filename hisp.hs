{-# LANGUAGE TupleSections #-}

{- HISP

Hisp is a:

* pure
* lazy
* functional
* reduced Lisp dialect

It has:

* recursion
* lambdas
* partial application

It doesn't have:

* loops
* (real) IO
* quoting
* macros

It wants:

* sharing <- need this badly
* aliasing
* `let`

-}

import Hisp.Eval
import Hisp.Base
import Hisp.Utils
import Hisp.Types
import Hisp.Scope
import Hisp.Parser
import Hisp.Builtins

import System.Environment (getArgs)
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe

import qualified Data.Map as M

---

main :: IO ()
main = do
  files <- getArgs
  scope <- load initialState files
  putStrLn "Welcome to the Hisp REPL. Type `h` for help."
  void $ runStateT run scope

-- | The bottom Scope is for lambdas. The next is for global functions.
initialState :: [Scope]
initialState = [builtins,M.empty]

-- | For now this will just load, and not execute any function calls.
load :: [Scope] -> [FilePath] -> IO [Scope]
load ss [] = return ss
load ss fs = do
  contents <- concat `fmap` mapM load' fs
  case parseExp ss contents of
    Left err      -> putStrLn ("Parsing Error:\n" ++ show err) >> return ss
    Right (_,ss') -> do
      let news = M.size (head ss') - M.size (head ss)
          suff = if news == 1 then "" else "s"
      putStrLn ("Done. Loaded " ++ show news ++ " function" ++ suff ++ ".")
      return ss'
    where load' f = putStrLn ("Loading " ++ f ++ "...") >> readFile f

run :: StateT [Scope] IO ()
run = forever $ do
  putStr' "> "
  input <- liftIO $ runMaybeT (getLine' >>= nest [])
  case input of
    Nothing  -> putStrLn' ">>> Too many right parentheses."
    Just []  -> return ()
    Just "h" -> help
    Just cs  -> get >>= \ss -> do
      let result = eval (parse cs >>= e) ss
      output <- case result of
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
                 , "     (define baz (a b) (+ a b))"
                 , "3. You can pass functions and also partially apply them:"
                 , "     (define $ (f a) (f a))"
                 , "     ($ sin 1)"
                 , "     ($ (+ 1) 1)"
                 , "     ($ (lambda (a) (+ 1 a)) 1)"
                 , "4. Feel free to write expressions over multiple lines." ]

parse :: String -> Evaluate Exp
parse s = get >>= \ss -> case parseExp ss s of
                           Left err -> failure $ "Syntax Error\n" ++ show err
                           Right (ex,ss') -> put ss' >> return (head ex)

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

parens :: [Int] -> String -> Maybe [Int]
parens = parens' 1

parens' :: Int -> [Int] -> String -> Maybe [Int]
parens' _ ps     []       = Just ps
parens' n ps     ('(':xs) = parens' (n + 1) (indent n xs : ps) xs
parens' n (_:ps) (')':xs) = parens' (n + 1) ps xs
parens' _ []     (')':_)  = Nothing
parens' n ps     (_:xs)   = parens' (n + 1) ps xs

-- | The position an argument should be indented to on the next line.
indent :: Int -> String -> Int
indent p []      = p
indent p (' ':_) = p
indent p ('(':_) = p - 1
indent p (_:ns)  = indent (p + 1) ns
