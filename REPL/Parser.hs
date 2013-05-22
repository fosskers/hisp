module REPL.Parser ( parseExp ) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.Parsec.Prim (Parsec)
import Control.Applicative

import REPL.Types

---

type REPLParser = Parsec String REPLState

parseExp :: REPLState -> String -> Either ParseError (Exp)
parseExp rs s = runParser (symbol <|> sexp) rs "(s-exp)" s

sexp :: REPLParser (Exp)
sexp = spaces *> char '(' *> prim <*> args <* char ')'

prim :: REPLParser ([Exp] -> Exp)
prim = (op <|> number) <* spaces

args :: REPLParser [Exp]
args = many1 ((sexp <|> symbol) <* spaces)

symbol :: REPLParser Exp
symbol = x <|> y <|> z <|> number'

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
x = char 'x' *> fmap (Val . head) getState

y :: REPLParser Exp
y = char 'y' *> fmap (Val . (!! 1)) getState

z :: REPLParser Exp
z = char 'z' *> fmap (Val . (!! 2)) getState