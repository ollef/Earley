{-# LANGUAGE RecursiveDo #-}
module Expr where
import Control.Applicative
import Data.Char
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Text.Earley

tests :: TestTree
tests = testGroup "Expr"
  [ QC.testProperty "Expr: parse . pretty = id" $
    \e -> [e] === parseExpr (prettyExpr 0 e)
  , QC.testProperty "Ambiguous Expr: parse . pretty ≈ id" $
    \e -> e `elem` parseAmbiguousExpr (prettyExpr 0 e)
  ]

parseExpr :: String -> [Expr]
parseExpr input = fst (fullParses (parser expr) (lexExpr input)) -- We need to annotate types for point-free version

parseAmbiguousExpr :: String -> [Expr]
parseAmbiguousExpr input = fst (fullParses (parser ambiguousExpr) (lexExpr input))

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Var String
  deriving (Eq, Ord, Show)

instance Arbitrary Expr where
  arbitrary = sized arbExpr
    where arbIdent           = Var <$> elements ["a", "b", "c", "x", "y", "z"]
          arbExpr n | n > 0  = oneof [ arbIdent
                                     , Add <$> arbExpr1 <*> arbExpr1
                                     , Mul <$> arbExpr1 <*> arbExpr1
                                     ]
                                     where arbExpr1 = arbExpr (n `div` 2)
          arbExpr _          = arbIdent

  shrink (Var _)    = []
  shrink (Add a b)  = a : b : [ Add a' b | a' <- shrink a ] ++ [ Add a b' | b' <- shrink b ]
  shrink (Mul a b)  = a : b : [ Mul a' b | a' <- shrink a ] ++ [ Mul a b' | b' <- shrink b ]

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

ambiguousExpr :: Grammar r (Prod r String String Expr)
ambiguousExpr = mdo
  x1 <- rule $ Add <$> x1 <* namedToken "+" <*> x1
            <|> x2
            <?> "sum"
  x2 <- rule $ Mul <$> x2 <* namedToken "*" <*> x2
            <|> x3
            <?> "product"
  x3 <- rule $ Var <$> (satisfy ident <?> "identifier")
            <|> namedToken "(" *> x1 <* namedToken ")"
  return x1
  where
    ident (x:_) = isAlpha x
    ident _     = False

prettyParens :: Bool -> String -> String
prettyParens True s  = "(" ++ s ++ ")"
prettyParens False s = s

prettyExpr :: Int -> Expr -> String
prettyExpr _ (Var s) = s
prettyExpr d (Add a b) = prettyParens (d > 0) $ prettyExpr 0 a ++ " + " ++ prettyExpr 1 b
prettyExpr d (Mul a b) = prettyParens (d > 1) $ prettyExpr 1 a ++ " * " ++ prettyExpr 2 b

-- @words@ like lexer, but consider parentheses as separate tokens
lexExpr :: String -> [String]
lexExpr ""        = []
lexExpr ('(' : s) = "(" : lexExpr s
lexExpr (')' : s) = ")" : lexExpr s
lexExpr (c : s)
  | isSpace c     = lexExpr s
  | otherwise     = let (tok, rest) = span p (c : s)
                    in tok : lexExpr rest
  where p x       = not (x == '(' || x == ')' || isSpace x)
