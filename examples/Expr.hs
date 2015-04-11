{-# LANGUAGE ScopedTypeVariables, RecursiveDo #-}
import Control.Applicative
import Data.Char
import System.Environment
import Text.Earley as E

data Expr
  = Expr :+: Expr
  | Expr :*: Expr
  | Var String
  | Lit Int
  deriving (Show)

grammar :: forall r. Grammar r (Prod r Char Expr)
grammar = mdo

  whitespace <- E.many $ satisfy isSpace
  identCont <- E.many $ satisfy isAlphaNum
  num       <- E.some (satisfy isDigit)

  let token :: Prod r Char a -> Prod r Char a
      token p  = whitespace *> p
      parens p = token (symbol '(') *> p <* token (symbol ')')
      ident    = (:) <$> satisfy isAlpha <*> identCont
      eident = token  ident
      enum   = token  num
      eplus  = token $ symbol '+'
      etimes = token $ symbol '*'

  expr0 <- rule
    [ (Lit . read)  <$> enum
    , Var  <$> eident
    , parens expr2
    ]

  expr1 <- rule
    [ (:*:) <$> expr1 <* etimes <*> expr0
    , expr0
    ]

  expr2 <- rule
    [ (:+:) <$> expr2 <* eplus <*> expr1
    , expr1
    ]
  return $ expr2 <* whitespace

main :: IO ()
main = do
  x:_ <- getArgs
  let n = read x
  print $ 4 + n * 21
  print $ length $ fullParses $ parser grammar ("asdf" ++ concat (replicate n "* 1213 + (232 + asdf)"))
