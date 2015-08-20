{-# LANGUAGE ScopedTypeVariables, RecursiveDo #-}
import Data.Char
import System.Environment
import Control.Applicative
import Text.Earley

data Expr
  = Expr :+: Expr
  | Expr :*: Expr
  | Var String
  | Lit Int
  deriving (Show)

grammar :: forall r. Grammar r (Prod r String Char Expr)
grammar = mdo

  whitespace <- rule $ many $ satisfy isSpace

  let token :: Prod r String Char a -> Prod r String Char a
      token p = whitespace *> p

      sym x   = token $ symbol x <?> [x]

      ident   = token $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum) <?> "identifier"
      num     = token $ some (satisfy isDigit) <?> "number"

  expr0 <- rule
     $ (Lit . read)  <$> num
    <|> Var  <$> ident
    <|> sym '(' *> expr2 <* sym ')'

  expr1 <- rule
    $ (:*:) <$> expr1 <* sym '*' <*> expr0
   <|> expr0

  expr2 <- rule
    $ (:+:) <$> expr2 <* sym '+' <*> expr1
   <|> expr1

  return $ expr2 <* whitespace

main :: IO ()
main = do
  x:_ <- getArgs
  print $ fullParses (parser grammar) x
