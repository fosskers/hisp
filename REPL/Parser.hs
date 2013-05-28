module REPL.Parser ( parseExp ) where

import Prelude hiding (lookup)

import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.Parsec.Prim (Parsec)
import Data.Map.Lazy    (lookup)
import Control.Applicative hiding (many)

import REPL.Types
import REPL.Builtins (voidFun)

---

type REPLParser = Parsec String REPLState

parseExp :: REPLState -> String -> Either ParseError Exp
parseExp rs = runParser (symbol <|> sexp) rs "(s-exp)"

sexp :: REPLParser Exp
sexp = spaces *> char '(' *> spaces *> funCall <*> args <* char ')'

funCall :: REPLParser ([Exp] -> Exp)
funCall = FunCall <$> function <* spaces

-- | Any set of characters in function position will be parsed
-- as a function call, but a special void function will be returned if
-- the parsed function doesn't actually exist.
function :: REPLParser Function
function = getState >>= builtinMap >>= \m -> do
  name <- many1 $ noneOf "\n() "
  case name `lookup` m of
    Nothing -> return $ voidFun name
    Just f  -> return f

args :: REPLParser [Exp]
args = many ((symbol <|> sexp) <* spaces)

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
