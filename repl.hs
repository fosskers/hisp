-- Reverse Polish Notation Calculator

import Calculator.Base
import Calculator.Parser

import Control.Monad (forever)
import System.IO (stdout, hFlush)

---

main :: IO ()
main = forever $ do
  putStr "> " >> hFlush stdout
  input <- getLine
  putStrLn $ ">>> " ++ show (rpn input)

rpn :: (Fractional a, Read a) => String -> a
rpn s = case parseExp s of
          Left err   -> error $ show err
          Right sexp -> e sexp

rpnFile :: (Fractional a, Read a) => FilePath -> IO a
rpnFile fp = rpn `fmap` readFile fp
