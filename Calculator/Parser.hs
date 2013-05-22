module Calculator.Parser ( parseExp ) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.Parsec.Prim (Parsec)
import Control.Applicative

import Calculator.Base

---

type REPLParser = Parsec String Value

parseExp :: Value -> String -> Either ParseError (Exp)
parseExp x s = runParser (symbol <|> sexp) x "(s-exp)" s

sexp :: REPLParser (Exp)
sexp = spaces *> char '(' *> prim <*> args <* char ')'

prim :: REPLParser ([Exp] -> Exp)
prim = (op <|> number) <* spaces

args :: REPLParser [Exp]
args = many1 ((sexp <|> symbol) <* spaces)

symbol :: REPLParser Exp
symbol = x <|> number'

number :: REPLParser ([Exp] -> Exp)
number = const `fmap` number'

number' :: REPLParser (Exp)
number' = do
  ds <- digits
  let val = if '.' `elem` ds
            then D $ (read ds :: Double)
            else I $ (read ds :: Integer)
  return $ Val val

digits :: REPLParser String
digits = (++) <$> whole <*> option "" dec
    where whole = many1 digit
          dec   = (:) <$> char '.' <*> whole

op :: REPLParser ([Exp] -> Exp)
op = Add <$ char '+'
     <|> Sub <$ char '-'
     <|> Mul <$ char '*'
     <|> Div <$ char '/'
     <|> Pow <$ char '^'
     <|> Fac <$ char '!'

x :: REPLParser Exp
x = char 'x' *> fmap Val getState
