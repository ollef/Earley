{-# LANGUAGE RecursiveDo #-}
module Lambda where
import Control.Applicative
import Data.List as List
import Data.Foldable
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC

import Text.Earley

import qualified Arbitrary

tests :: TestTree
tests = testGroup "Lambda"
  [ HU.testCase "Generate exactly 0" $
      exactly 0 gen @?= []
  , HU.testCase "Generate upTo 0" $
      upTo 0 gen @?= []
  , HU.testCase "Generate exactly 4" $
      sort (snd <$> exactly 4 gen)
      @?=
      ["(a)a","(a)b","(aa)","(ab)","(b)a","(b)b","(ba)","(bb)"
      ,"\\a.a","\\a.b","\\b.a","\\b.b","a(a)","a(b)","a+aa","a+ab"
      ,"a+ba","a+bb","aa+a","aa+b","aaaa","aaab","aaba","aabb"
      ,"ab+a","ab+b","abaa","abab","abba","abbb","b(a)","b(b)"
      ,"b+aa","b+ab","b+ba","b+bb","ba+a","ba+b","baaa","baab"
      ,"baba","babb","bb+a","bb+b","bbaa","bbab","bbba","bbbb"
      ]
  , HU.testCase "upTo contains exactly" $ List.and (do
      m <- [0..5]
      let ys = snd <$> upTo m gen
      n <- [0..m]
      (_, x) <- upTo n gen
      return $ x `List.elem` ys)
    @? "exactly contains upTo"
  , HU.testCase "language contains upTo" $ do
    let ys = snd <$> language gen
    List.and (do
      n <- [0..5]
      (_, x) <- upTo n gen
      return $ x `List.elem` ys)
    @? "exactly contains upTo"
  , QC.testProperty "Arbitrary" $ do
    let p = parser grammar
    (e, s) <- Arbitrary.arbitrary $ generator grammar tokens
    return
      $ [e] === fst (fullParses p $ prettyExpr 0 e)
      .&&. [e] === fst (fullParses p s)
  ]
  where
    gen = generator grammar tokens

data Expr
  = Var Char
  | Lam String Expr
  | App Expr Expr
  | Add Expr Expr
  deriving (Eq, Ord, Show)

prettyExpr :: Int -> Expr -> String
prettyExpr _ (Var c) = [c]
prettyExpr d (Lam xs e) = prettyParens (d > 0) $ "\\" ++ xs ++ "." ++ prettyExpr d e
prettyExpr d (Add a b) = prettyParens (d > 1) $ prettyExpr 2 a ++ "+" ++ prettyExpr 1 b
prettyExpr d (App a b) = prettyParens (d > 3) $ prettyExpr 3 a ++ prettyExpr 4 b

prettyParens :: Bool -> String -> String
prettyParens True s  = "(" ++ s ++ ")"
prettyParens False s = s

tokens :: String
tokens = "(\\ab.+*)"

instance Arbitrary Expr where
  arbitrary = sized go
    where
      var = elements "ab"
      go 0 = Var <$> var
      go n = oneof
        [ Var <$> var
        , Lam <$> (take 2 <$> listOf1 var) <*> go'
        , App <$> go' <*> go'
        , Add <$> go' <*> go'
        ]
        where
          go' = go (n `div` 10)

  shrink (Var _) = []
  shrink (Lam xs e) = e : [Lam xs' e' | xs' <- shrink xs, not (null xs), e' <- shrink e]
  shrink (App a b) = a : b : [App a' b' | a' <- shrink a, b' <- shrink b]
  shrink (Add a b) = a : b : [Add a' b' | a' <- shrink a, b' <- shrink b]

grammar :: Grammar r m (Prod r m String Char Expr)
grammar = mdo
  let v = asum (token <$> "ab")
        <?> "variable"
  x1 <- rule
    $ Lam <$ token '\\' <*> some v <* token '.' <*> x1
    <|> x2
    <?> "lambda"
  x2 <- rule
    $ Add <$> x3 <* token '+' <*> x2
    <|> x3
    <?> "sum"
  x3 <- rule
    $ App <$> x3 <*> x4
    <|> x4
    <?> "application"
  let x4 = Var <$> v
        <|> token '(' *> x1 <* token ')'
  return x1
