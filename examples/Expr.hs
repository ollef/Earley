{-# LANGUAGE RecursiveDo #-}
import Control.Applicative
import Data.Char
import System.Environment
import Text.Earley

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Var String
  deriving (Eq, Ord, Show)

expr :: Grammar r (Prod r String String Expr)
expr = mdo
  x1 <- rule $ Add <$> x1 <* namedToken "+" <*> x2
            <|> x2
            <?> "sum"
  x2 <- rule $ Mul <$> x2 <* namedToken "*" <*> x3
            <|> x3
            <?> "product"
  x3 <- rule $ Var <$> (satisfy ident <?> "identifier")
            <|> namedToken "(" *> x1 <* namedToken ")"
  return x1
  where
    ident (x:_) = isAlpha x
    ident _     = False

main :: IO ()
main = do
  x:_ <- getArgs
  print $ fullParses (parser expr) $ words x
