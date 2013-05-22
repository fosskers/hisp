-- Simple s-expression repl.

import Calculator.Base
import Calculator.Parser

import System.IO (stdout, hFlush)
import Control.Monad

---

main :: IO ()
main = forever $ do
  putStr' "> "
  input <- getLine >>= nest []
  unless (null input) $
         putStrLn $ ">>> " ++ case rpn input of
                                Left err -> err
                                Right v  -> show v

rpn :: String -> Either String Value
rpn s = case parseExp s of
          Left err   -> Left $ show err
          Right sexp -> e sexp

-- Only notes the locations of left parens.
-- Convert this to `MaybeT IO String`?
nest :: [Int] -> String -> IO String
nest ps line =
  case parens ps line of
    Nothing  -> error "Too many right parentheses."
    Just []  -> return line
    Just ps' -> do
      let pos = head ps' + 2
          pad = replicate pos ' '
      putStr' $ ".  " ++ pad
      input <- (('\n' : pad) ++) `fmap` getLine
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

putStr' :: String -> IO ()
putStr' s = putStr s >> hFlush stdout
