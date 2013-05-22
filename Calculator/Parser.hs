module Calculator.Parser ( parseExp ) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative

import Calculator.Base

---

parseExp :: String -> Either ParseError (Exp)
parseExp = parse sexp "(s-exp)"

sexp :: Parser (Exp)
sexp = spaces *> char '(' *> prim <*> args <* char ')'

prim :: Parser ([Exp] -> Exp)
prim = (op <|> number) <* spaces

args :: Parser [Exp]
args = many1 ((sexp <|> number') <* spaces)

number :: Parser ([Exp] -> Exp)
number = const `fmap` number'

number' :: Parser (Exp)
number' = do
  ds <- digits
  let val = if '.' `elem` ds
            then D $ (read ds :: Double)
            else I $ (read ds :: Integer)
  return $ Val val

digits :: Parser String
digits = (++) <$> whole <*> option "" dec
    where whole = many1 digit
          dec   = (:) <$> char '.' <*> whole

op :: Parser ([Exp] -> Exp)
op = Add <$ char '+'
     <|> Sub <$ char '-'
     <|> Mul <$ char '*'
     <|> Div <$ char '/'
     <|> Pow <$ char '^'
     <|> Fac <$ char '!'
