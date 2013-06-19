{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

{- HISP

Hisp is a:

* pure
* lazy
* functional
* reduced Lisp dialect

It has:

* recursion
* lambdas
* few special forms
* a simple module system

It doesn't have:

* loops
* (real) IO
* quoting
* macros

It wants:

* sharing <- need this badly
* `let`

-}

import Hisp.Eval
import Hisp.Base
import Hisp.Flags
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

type StateIO s = StateT s IO

main :: IO ()
main = do
  args <- getArgs
  void $ case parseFlags args of
    ([],[])      -> runStateT repl initialState
    ([],inp)     -> runStateT (run inp) initialState
    ([Load],inp) -> load initialState inp >>= runStateT repl
    ([Exec],inp) -> runStateT (rep $ concat inp) initialState

-- | The bottom Scope is for lambdas. The next is for global functions.
initialState :: [Scope]
initialState = [builtins,M.empty]

-- | Only run one file.
run :: [FilePath] -> StateIO [Scope] ()
run []     = return ()
run (f:_) = do
  es <- parseFile f
  case es of
    [] -> return ()
    _  -> get >>= \ss -> do
      let es' = filter isList es
      liftIO $ forM_ es' (\ex -> eval (e ex) ss >>=
                                 \case
                                   (Left err,_) -> putStrLn $ errMsg err
                                   (Right v,_)  -> print v)

-- | For now this will just load, and not execute any function calls.
-- CONVERT THIS TO PARSEFILE.
load :: [Scope] -> [FilePath] -> IO [Scope]
load ss [] = return ss
load ss fs = do
  contents <- concat `fmap` mapM load' fs
  case parseExp "" ss contents of
    Left err      -> putStrLn ("Parsing Error:\n" ++ show err) >> return ss
    Right (_,ss') -> do
      let news = M.size (head ss') - M.size (head ss)
          suff = if news == 1 then "" else "s"
      putStrLn ("Done. Loaded " ++ show news ++ " function" ++ suff ++ ".")
      return ss'
    where load' f = putStrLn ("Loading " ++ f ++ "...") >> readFile f

repl :: StateIO [Scope] ()
repl = do
  liftIO (putStrLn "Welcome to the Hisp REPL. Type `h` for help.")
  forever $ do
    putStr' "> "
    input <- liftIO $ runMaybeT (getLine' >>= nest [])
    case input of
      Nothing  -> putStrLn' ">>> Too many right parentheses."
      Just []  -> return ()
      Just "h" -> help
      Just cs  -> rep cs

rep :: String -> StateIO [Scope] ()
rep cs = get >>= \ss -> do
--let result = eval (parse cs >>= e) ss
  result <- liftIO $ eval (parse cs >>= e) ss
  output <- case result of
              (Left err, _)  -> return $ errMsg err
              (Right v, ss') -> put ss' >> inject v >> return (show v)
  putStrLn' $ ">>> " ++ output

parseFile :: FilePath -> StateIO [Scope] [Exp]
parseFile f = parseFile' [f] [] []

parseFile' :: [FilePath] -> [FilePath] -> [Exp] -> StateIO [Scope] [Exp]
parseFile' [] _ es        = return es
parseFile' (f:fs) done es = get >>= \ss -> do
  contents <- liftIO $ readFile f
  case parseExp f ss contents of
    Left err -> liftIO (putStrLn ("Parsing Error:\n" ++ show err)) >> return []
    Right (es',ss') -> do
      let imports = filter (`notElem` done) . map reqPath . filter isRequire $ es'
      put ss' >> parseFile' (imports ++ fs) (f : done) (es' ++ es)

help :: StateIO [Scope] ()
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
                 , "     ($ (lambda (a) (+ 1 a)) 1)"
                 , "4. Feel free to write expressions over multiple lines." ]

parse :: String -> Evaluate Exp
parse s = get >>= \ss -> case parseExp "" ss s of
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
