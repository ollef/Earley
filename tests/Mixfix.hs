module Mixfix where
import Control.Applicative
import Test.Tasty
import Test.Tasty.HUnit as HU

import Text.Earley
import Text.Earley.Mixfix

tests :: TestTree
tests = testGroup "Mixfix"
  [ HU.testCase "1" $
      let x = Ident [Just "x"] in
      fullParses (parser mixfixGrammar) (words "if x then x else x")
      @?= (,) [App ifthenelse [x, x, x]] Report {position = 6, expected = [], unconsumed = []}
  , HU.testCase "2" $
      let x = Ident [Just "x"] in
      fullParses (parser mixfixGrammar) (words "prefix x postfix")
      @?= (,) [App prefix [App postfix [x]]] Report {position = 3, expected = [], unconsumed = []}
  , HU.testCase "3" $
      let x = Ident [Just "x"] in
      fullParses (parser mixfixGrammar) (words "x infix1 x infix2 x")
      @?= (,) [App infix1 [x, App infix2 [x, x]]] Report {position = 5, expected = [], unconsumed = []}
  , HU.testCase "4" $
      let x = Ident [Just "x"] in
      fullParses (parser mixfixGrammar) (words "[ x ]")
      @?= (,) [App closed [x]] Report {position = 3, expected = [], unconsumed = []}
  ]

data MixfixExpr = Ident (Holey String) | App (Holey String) [MixfixExpr]
  deriving (Eq, Show)

mixfixGrammar :: Grammar r m (Prod r m String String MixfixExpr)
mixfixGrammar = mixfixExpression table
                                 (Ident . pure . Just <$> namedToken "x")
                                 App
  where
    hident = map (fmap token)
    table =
      [ [(hident ifthenelse, RightAssoc)]
      , [(hident prefix, RightAssoc)]
      , [(hident postfix, LeftAssoc)]
      , [(hident infix1, LeftAssoc)]
      , [(hident infix2, RightAssoc)]
      , [(hident closed, NonAssoc)]
      ]

ifthenelse, prefix, postfix, infix1, infix2, closed :: Holey String
ifthenelse = [Just "if", Nothing, Just "then", Nothing, Just "else", Nothing]
prefix = [Just "prefix", Nothing]
postfix = [Nothing, Just "postfix"]
infix1 = [Nothing, Just "infix1", Nothing]
infix2 = [Nothing, Just "infix2", Nothing]
closed = [Just "[", Nothing, Just "]"]
