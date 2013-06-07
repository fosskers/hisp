{-# LANGUAGE TupleSections #-}

module Hisp.Parser where  --( parseExp ) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative           hiding (many)
import Prelude                       hiding (lookup)
import Text.Parsec.Prim (Parsec, modifyState)
import Data.Hashable    (hash)
import Control.Arrow    (first)

import Hisp.Base (newLambda, newGlobal)
import Hisp.Eval (e)
import Hisp.Types

---

type HispParser = Parsec String [Scope]

parseExp :: [Scope] -> String -> Either ParseError ([Exp],[Scope])
parseExp rs = runParser ((,) <$> many1 (atom <* spaces) <*> getState) rs "(s-exp)"

atom :: HispParser Exp
atom = symbol <|> sexp  -- <|> list

sexp :: HispParser Exp
sexp = spaces *> char '(' *> spaces *> (define <|> lambda <|> call) <* char ')'

call :: HispParser Exp
call = flip Call noHash <$> (function <* spaces) <*> args

-- | Any set of characters in function position will be parsed
-- as a function call, but a special void function will be returned if
-- the parsed function doesn't actually exist.
function :: HispParser String
function = many1 $ noneOf "\n()[] "

args :: HispParser [Exp]
args = many (atom <* spaces)

-- The `noHash` here shouldn't be a problem. Global symbols will be called by
-- name, and symbols within function definitions will have their hashes
-- altered to match those of the function parameters.
symbol :: HispParser Exp
symbol = number <|> boolean <|> (function >>= \f -> return (Call f noHash []))

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
boolean = (Val $ B True)  <$ string "True"
      <|> (Val $ B False) <$ string "False"


lambda :: HispParser Exp
lambda = do
  string "lambda" >> spaces
  (ps,body) <- first (map (,noHash)) `fmap` functionBody
  let name  = "lambda"
      hash' = hash $ show body  -- Should be unique enough.
      func  = Function name hash' (Just body) (Exactly (length ps) ps) (const $ e body)
  modifyState $ newLambda func
  return $ Call "lambda" hash' []

define :: HispParser Exp
define = do
  string "define" >> spaces
  name <- many1 (noneOf "()\n ") <* spaces
  (ps,body) <- first (map (,noHash)) `fmap` functionBody
  let hash' = hash $ name : map fst ps  -- Might be unique enough.
      func  = Function name hash' (Just body) (Exactly (length ps) ps) (const $ e body)
  modifyState $ newGlobal func
  return . Val . I . fromIntegral $ hash'

functionBody :: HispParser ([String],Exp)
functionBody = (,) <$> (option [] params <* spaces) <*> (atom <* spaces)

params :: HispParser [String]
params = char '[' *> spaces *> many (many1 (noneOf "\n[] ") <* spaces) <* char ']'

--list :: HispParser Exp
--list = char '[' *> spaces *> ((Val . L) <$> args) <* char ']'
