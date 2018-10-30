{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
module UnbalancedPars where

import Data.Char (isAlpha)
import Data.List (sortBy)
import Data.Ord (comparing)

import Control.Applicative
import Test.Tasty
import Test.Tasty.HUnit      as HU

import Text.Earley

tests :: TestTree
tests = testGroup "Unbalanced parentheses"
  [ HU.testCase "Parses balanced" $
      (fst $ fullParses (parser unbalancedPars) $
        "((x))") @?= [(b . b) x]
  , HU.testCase "Parses one unbalanced" $
      (fst $ fullParses (parser unbalancedPars) $
        "((x)") @?= [(u . b) x]
  , HU.testCase "Parses two unbalanced" $
      (fst $ fullParses (parser unbalancedPars) $
        "((x") @?= [(u . u) x]
  , HU.testCase "Generator up to 4" $
      let actual =
            upTo 5 $ generator unbalancedPars ")(x"
          model =
            map (\mk -> let e = mk x in (e, renderExpr e))
              [ id, u, b, u.u, b.b, u.b, b.u,
                u.u.u, u.u.b, u.b.u, b.u.u ] -- (>2b + u) don't fit in 5 chars
      in normalizeGenerated actual @?= normalizeGenerated model
  ]

-- We don't care about the particular order of generated inputs, so we
-- normalize before the comparison.
normalizeGenerated :: [(Expr, String)] -> [(Expr, String)]
normalizeGenerated = sortBy (comparing fst)

data Expr =
  Var Char | ExprInBrackets String Expr String
  deriving (Eq, Ord, Show)

renderExpr :: Expr -> String
renderExpr (Var c) = [c]
renderExpr (ExprInBrackets l e r) = l ++ renderExpr e ++ r

-- [b]alanced
b :: Expr -> Expr
b e = ExprInBrackets "(" e ")"

-- [u]nbalanced
u :: Expr -> Expr
u e = ExprInBrackets "(" e ""

-- [x] variable
x :: Expr
x = Var 'x'

unbalancedPars :: Grammar r (Prod r String Char Expr)
unbalancedPars = mdo
  var <- rule $ terminal $
    \c -> if isAlpha c then Just (Var c) else Nothing
  expr <- rule $ var <|> exprInBrackets
  leftPar <- rule $ terminal $
    \c -> case c of
      '(' -> Just [c]
      _ -> Nothing
  rightPar <- rule $ terminal $
    \c -> case c of
      ')' -> Just [c]
      _ -> Nothing
  exprInBrackets <- rule $
    ExprInBrackets
      <$> leftPar
      <*> expr
      <*> (rightPar <|> ("" <$ eof))
      <?> "parenthesized expression"
  return expr
