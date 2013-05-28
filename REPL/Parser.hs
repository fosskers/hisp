module REPL.Parser ( parseExp ) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative           hiding (many)
import Prelude                       hiding (lookup)
import Text.Parsec.Prim (Parsec, modifyState)
import Data.Map.Lazy    (lookup,insert)

import REPL.Eval     (e)
import REPL.Builtins (voidFun)
import REPL.Types

---

type REPLParser = Parsec String Scope

parseExp :: Scope -> String -> Either ParseError (Exp,Scope)
parseExp rs = runParser ((,) <$> atom <*> getState) rs "(s-exp)"

atom :: REPLParser Exp
atom = symbol <|> sexp

sexp :: REPLParser Exp
sexp = spaces *> char '(' *> spaces *> (define <|> funCall) <* char ')'

funCall :: REPLParser Exp
funCall = FunCall <$> (function <* spaces) <*> args

-- | Any set of characters in function position will be parsed
-- as a function call, but a special void function will be returned if
-- the parsed function doesn't actually exist.
function :: REPLParser Function
function = getState >>= scope >>= \s -> do
  name <- many1 $ noneOf "\n() "
  case name `lookup` s of
    Nothing -> return $ voidFun name
    Just f  -> return f

args :: REPLParser [Exp]
args = many (atom <* spaces)

symbol :: REPLParser Exp
symbol = number <|> (flip FunCall [] <$> function)

number :: REPLParser Exp
number = do
  ds <- digits
  let val = if '.' `elem` ds
            then D (read ds :: Double)
            else I (read ds :: Integer)
  return $ Val val

digits :: REPLParser String
digits = (++) <$> whole <*> option "" dec
    where whole = many1 digit
          dec   = (:) <$> char '.' <*> whole

define :: REPLParser Exp
define = do
  string "define" >> spaces
  name  <- many1 (noneOf "()\n ") <* spaces
  inner <- atom
  modifyState $ insert name $ Function name None (const $ e inner)
  return $ Val 1
