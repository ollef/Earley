{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}

import Control.Applicative
import Data.Char
import Data.Foldable (traverse_)
import Data.Tree
import System.Environment
import Text.Earley
import Text.Earley.Grammar

type Expr = Tree String
pattern Add, Mul :: Tree String -> Tree String -> Tree String
pattern Add x y = Node "+" [x, y]
pattern Mul x y = Node "*" [x, y]
pattern Var :: a -> Tree a
pattern Var n = Node n []
pattern Amb :: [Tree String] -> Tree String
pattern Amb xs = Node "Ambiguous" xs

expr :: Grammar r (Prod r String String Expr)
expr = mdo
  let exprProd = disambiguate $ \case
        [x] -> x
        xs -> Amb xs
  e <-
    exprProd $
      Add <$> e <* namedToken "+" <*> e
        <|> Mul <$> e <* namedToken "*" <*> e
        <|> Var <$> satisfy ident
        <|> namedToken "(" *> e <* namedToken "("
  return e
 where
  ident (x : _) = isAlpha x
  ident _ = False

-- λ> :main "A + B * C * G"
-- Ambiguous
-- ├╴ ((A+B)*(C*G))
-- ├╴ *
-- │  ├╴ Ambiguous
-- │  │  ├╴ ((A+B)*C)
-- │  │  └╴ (A+(B*C))
-- │  └╴ G
-- └╴ +
--    ├╴ A
--    └╴ Ambiguous
--       ├╴ (B*(C*G))
--       └╴ ((B*C)*G)
main :: IO ()
main = do
  x : _ <- getArgs
  let (ps, r) = fullParses (parser expr) (words x)
  traverse_ (putStrLn . drawTree . simplifyTree) ps
  print r

-- | render non-ambiguous expressions on one line to make the printed tree
-- smaller
simplifyTree :: Expr -> Expr
simplifyTree =
  foldTree
    ( \op -> \case
        [Node n [], Node m []] | op /= "Ambiguous" -> Node (parens (n <> op <> m)) []
        ns -> Node op ns
    )
 where
  parens x = "(" <> x <> ")"
