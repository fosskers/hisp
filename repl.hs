-- Simple s-expression repl.

import Calculator.Base
import Calculator.Parser

import System.IO (stdout, hFlush)
import Control.Monad

---

main :: IO ()
main = forever $ do
  putStr' "> "
  input <- getLine >>= nest [] []
  unless (null input) $
         putStrLn $ ">>> " ++ case rpn input of
                                Left err -> err
                                Right v  -> show v

rpn :: String -> Either String Value
rpn s = case parseExp s of
          Left err   -> Left $ show err
          Right sexp -> e sexp

-- Runs on the assumption that operators are only 1 char long.
-- This could be better.
nest :: [Int] -> [Int] -> String -> IO String
nest ls rs line = do
  let (ls',rs')   = lsAndRs line
      (ls'',rs'') = (ls ++ ls', rs ++ rs')
  case compare (length ls'') (length rs'') of
    EQ -> return line
    LT -> error "Too many right parentheses."
    GT -> do
      let pos = ls'' !! (length ls'' - length rs'' - 1)
          pad = replicate (pos + 2) ' '
      putStr' $ ".  " ++ pad
      input <- (('\n' : pad) ++) `fmap` getLine
      (line ++) `fmap` nest ls'' rs'' input

lsAndRs :: String -> ([Int],[Int])
lsAndRs line = dropFst $ foldr fold (length line - 1,[],[]) line
    where fold '(' (n,ls,rs) = (n - 1, n : ls, rs)
          fold ')' (n,ls,rs) = (n - 1, ls, n : rs)
          fold _   (n,ls,rs) = (n - 1, ls, rs)

dropFst :: (a,b,c) -> (b,c)
dropFst (_,b,c) = (b,c)

putStr' :: String -> IO ()
putStr' s = putStr s >> hFlush stdout
