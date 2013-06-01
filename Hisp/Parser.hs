module Hisp.Parser where  --( parseExp ) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative           hiding (many)
import Prelude                       hiding (lookup)
import Text.Parsec.Prim (Parsec, modifyState)
import Data.Hashable    (hash)

import Hisp.Base (newLambda, newGlobal)
import Hisp.Eval (e)
import Hisp.Types

---

type HispParser = Parsec String [Scope]

parseExp :: [Scope] -> String -> Either ParseError (Exp,[Scope])
parseExp rs = runParser ((,) <$> atom <*> getState) rs "(s-exp)"

atom :: HispParser Exp
atom = symbol <|> sexp <|> list

sexp :: HispParser Exp
sexp = spaces *> char '(' *> spaces *> (define <|> lambda <|> funCall) <* char ')'

funCall :: HispParser Exp
funCall = Call <$> (function <* spaces) <*> args

-- | Any set of characters in function position will be parsed
-- as a function call, but a special void function will be returned if
-- the parsed function doesn't actually exist.
function :: HispParser String
function = many1 $ noneOf "\n()[] "

args :: HispParser [Exp]
args = many (atom <* spaces)

symbol :: HispParser Exp
symbol = number <|> boolean <|> (flip Call [] <$> function)

number :: HispParser Exp
number = do
  ds <- digits
  let val = if '.' `elem` ds
            then D (read ds :: Double)
            else I (read ds :: Integer)
  return $ Val val

digits :: HispParser String
digits = (++) <$> whole <*> option "" dec
    where whole = many1 digit
          dec   = (:) <$> char '.' <*> whole

boolean :: HispParser Exp
boolean = (Val $ B True) <$ string "True"
          <|> (Val $ B False) <$ string "False"


lambda :: HispParser Exp
lambda = do
  string "lambda" >> spaces
  (ps,body) <- functionBody
  let ps'  = unwords ps
      name = "lambda [" ++ ps' ++ "] @ " ++ show (hash $ show body)
      func = Function name (Exactly (length ps) ps) (const $ e body)
  modifyState $ newLambda func
  return $ Call name []

define :: HispParser Exp
define = do
  string "define" >> spaces
  name <- many1 (noneOf "()\n ") <* spaces
  (ps,body) <- functionBody
  let func = Function name (Exactly (length ps) ps) (const $ e body)
  modifyState $ newGlobal func
  return $ Val 1

functionBody :: HispParser ([String],Exp)
functionBody = (,) <$> (option [] params <* spaces) <*> (atom <* spaces)

params :: HispParser [String]
params = char '[' *> spaces *> many (many1 (noneOf "\n[] ") <* spaces) <* char ']'

list :: HispParser Exp
list = char '[' *> spaces *> ((Val . L) <$> args) <* char ']'
