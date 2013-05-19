module Calculator.Parser ( parseExp ) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative

import Calculator.Base

---

parseExp :: (Read a, Num a) => String -> Either ParseError (Exp a)
parseExp = parse sexp "(s-exp)"

sexp :: (Read a, Num a) => Parser (Exp a)
sexp = spaces *> char '(' *> prim <*> args <* char ')'

prim :: (Read a, Num a) => Parser ([Exp a] -> Exp a)
prim = (op <|> number) <* spaces

args :: (Read a, Num a) => Parser [Exp a]
args = many1 ((sexp <|> number') <* spaces)

number :: (Read a, Num a) => Parser ([Exp a] -> Exp a)
number = const `fmap` number'

number' :: (Read a, Num a) => Parser (Exp a)
number' = (Val . read) <$> digits

digits :: Parser String
digits = (++) <$> whole <*> option "" dec
    where whole = many1 digit
          dec   = (:) <$> char '.' <*> whole

op :: Num a => Parser ([Exp a] -> Exp a)
op = Add <$ char '+'
     <|> Sub <$ char '-'
     <|> Mul <$ char '*'
     <|> Div <$ char '/'
