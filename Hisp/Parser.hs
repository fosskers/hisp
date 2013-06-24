{-# LANGUAGE TupleSections #-}

module Hisp.Parser where  --( parseExp ) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative           hiding (many)
import Prelude                       hiding (lookup)
import Text.Parsec.Prim (Parsec, modifyState)
import Data.Hashable    (hash)

import Hisp.Scope
import Hisp.Base (symbolString)
import Hisp.Eval (e)
import Hisp.Types

---

type HispParser = Parsec String [Scope]

parseExp :: FilePath -> [Scope] -> String -> Either ParseError ([Exp],[Scope])
parseExp f ss = runParser ((,) <$> hisp <*> getState) ss f

hisp :: HispParser [Exp]
hisp = spaces *> many1 (comment <|> atom)

comment :: HispParser Exp
comment = char ';' *> (Comment <$ many (noneOf "\n")) <* spaces

atom :: HispParser Exp
atom = (datum <|> sexp) <* spaces

datum :: HispParser Exp
datum = number <|> boolean <|> oneChar <|> chars <|> symbol

number :: HispParser Exp
number = do
  ds <- digits
  let val = if '.' `elem` ds
            then N $ D (read ds :: Double)
            else N $ I (read ds :: Integer)
  return $ Val val

digits :: HispParser String
digits = (++) <$> whole <*> option "" dec
    where whole = many1 digit
          dec   = (:) <$> char '.' <*> whole

boolean :: HispParser Exp
boolean = (Val $ B True)  <$ string "True"
      <|> (Val $ B False) <$ string "False"

symbol :: HispParser Exp
symbol = flip Symbol noHash <$> (many1 $ noneOf "\n()[] ")

oneChar :: HispParser Exp
oneChar = char '\'' *> (Val . C <$> noneOf "'") <* char '\''

special :: HispParser Char
special = do
  char '\\'
  cs <- many1 $ noneOf "\n"
  return $ read ('\'' : cs ++ "'")

-- Which is of course, a String.
-- Doesn't seem to allow multiline strings.
chars :: HispParser Exp
chars = char '"' *> (List . map (Val . C) <$> many (noneOf "\n\"")) <* char '"'

sexp :: HispParser Exp
sexp = char '(' *> spaces *> (function <|> require <|> list) <* char ')'

function :: HispParser Exp
function = try (define <|> lambda)

list :: HispParser Exp
list = List <$> many atom

----------------
-- SPECIAL FORMS
----------------
define :: HispParser Exp
define = do
  string "define" >> spaces
  name <- (symbol <* spaces) >>= symbolString
  (ps,body) <- functionBody
  let ps' = expList ps
  hash' <- (hash . (name :)) `fmap` mapM symbolString ps'
  let func = Function name hash' (Just body)
             (Exactly (length ps') ps') (const $ e body)
  modifyState $ newGlobal func
  return $ Symbol name hash'

functionBody :: HispParser (Exp,Exp)
functionBody = (,) <$> (option (List []) sexp <* spaces) <*> atom

lambda :: HispParser Exp
lambda = do
  (string "lambda" <|> string "λ") >> spaces
  (ps,body) <- functionBody
  let ps' = expList ps
      name  = "λ"
      hash' = hash $ show body  -- Should be unique enough. Inefficient?
      func  = Function name hash' (Just body)
              (Exactly (length ps') ps') (const $ e body)
  modifyState $ newLambda func
  return $ Symbol name hash'

require :: HispParser Exp
require = try $ string "require" >> spaces >> (Require . show) `fmap` chars
