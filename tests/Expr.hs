{-# LANGUAGE RecursiveDo #-}
module Expr where
import Control.Applicative
import Data.Char
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Text.Earley
import Text.Earley.Generator

import qualified Arbitrary

tests :: TestTree
tests = testGroup "Expr"
  [ QC.testProperty "Left-recursive: parse . pretty = id" $
    \e -> [e] === parseLeftExpr (prettyLeftExpr 0 e)
  , QC.testProperty "Left-recursive: parse . pretty = id (generator)" $ do
    (e, s) <- Arbitrary.arbitrary $ generator leftExpr tokens
    return
      $ [e] === parseLeftExpr (prettyLeftExpr 0 e)
      .&&. [e] === parseLeftExpr (unwords s)
  , QC.testProperty "Right-recursive: parse . pretty = id" $
    \e -> [e] === parseRightExpr (prettyRightExpr 0 e)
  , QC.testProperty "Right-recursive: parse . pretty = id (generator)" $ do
    (e, s) <- Arbitrary.arbitrary $ generator rightExpr tokens
    return
      $ [e] === parseRightExpr (prettyRightExpr 0 e)
      .&&. [e] === parseRightExpr (unwords s)
  , QC.testProperty "Ambiguous: parse . pretty ≈ id" $
    \e -> e `elem` parseAmbiguousExpr (prettyLeftExpr 0 e)
      .&&. e `elem` parseAmbiguousExpr (prettyRightExpr 0 e)
      .&&. [e] == parseAmbiguousExpr (prettyAmbiguousExpr e)
  , QC.testProperty "Ambiguous: parse . pretty ≈ id (generator)" $ do
    (e, s) <- Arbitrary.arbitrary $ generator ambiguousExpr tokens
    return $ e `elem` parseAmbiguousExpr (prettyLeftExpr 0 e)
      .&&. e `elem` parseAmbiguousExpr (prettyRightExpr 0 e)
      .&&. [e] == parseAmbiguousExpr (prettyAmbiguousExpr e)
      .&&. e `elem` parseAmbiguousExpr (unwords s)
  ]

tokens :: [String]
tokens = pure <$> "abcxyz+*()"

parseLeftExpr :: String -> [Expr]
parseLeftExpr input = fst (fullParses (parser leftExpr) (lexExpr input))

parseRightExpr :: String -> [Expr]
parseRightExpr input = fst (fullParses (parser rightExpr) (lexExpr input))

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

leftExpr :: Grammar r (Prod r String String Expr)
leftExpr = mdo
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

rightExpr :: Grammar r (Prod r String String Expr)
rightExpr = mdo
  x1 <- rule $ Add <$> x2 <* namedToken "+" <*> x1
            <|> x2
            <?> "sum"
  x2 <- rule $ Mul <$> x3 <* namedToken "*" <*> x2
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

prettyLeftExpr :: Int -> Expr -> String
prettyLeftExpr _ (Var s) = s
prettyLeftExpr d (Add a b) = prettyParens (d > 0) $ prettyLeftExpr 0 a ++ " + " ++ prettyLeftExpr 1 b
prettyLeftExpr d (Mul a b) = prettyParens (d > 1) $ prettyLeftExpr 1 a ++ " * " ++ prettyLeftExpr 2 b

prettyRightExpr :: Int -> Expr -> String
prettyRightExpr _ (Var s) = s
prettyRightExpr d (Add a b) = prettyParens (d > 0) $ prettyRightExpr 1 a ++ " + " ++ prettyRightExpr 0 b
prettyRightExpr d (Mul a b) = prettyParens (d > 1) $ prettyRightExpr 2 a ++ " * " ++ prettyRightExpr 1 b

prettyAmbiguousExpr :: Expr -> String
prettyAmbiguousExpr (Var s) = s
prettyAmbiguousExpr (Add a b) = prettyParens True $ prettyAmbiguousExpr a ++ " + " ++ prettyAmbiguousExpr b
prettyAmbiguousExpr (Mul a b) = prettyParens True $ prettyAmbiguousExpr a ++ " * " ++ prettyAmbiguousExpr b

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
