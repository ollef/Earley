{-# LANGUAGE RecursiveDo #-}
module Issue11 where
import Control.Applicative
import Test.Tasty
import Test.Tasty.HUnit as HU

import Text.Earley
import Text.Earley.Mixfix

tests :: TestTree
tests = testGroup "Issue 11"
  [ let x = words "+ + 5 6 7" in
    HU.testCase "1" $
    fullParses (parser $ grammar LeftAssoc) x
    @?= (,) [] Report {position = 1, expected = [], unconsumed = drop 1 x}
  , let x = words "+ 5 + 6 7" in
    HU.testCase "2" $
    fullParses (parser $ grammar LeftAssoc) x
    @?= (,) [] Report {position = 2, expected = [], unconsumed = drop 2 x}
  , let x = words "+ 5 6" in
    HU.testCase "3" $
    fullParses (parser $ grammar LeftAssoc) x
    @?= (,) [Plus (Var "5") (Var "6")]
            Report {position = 3, expected = [], unconsumed = []}
  , let x = words "+ + 5 6 7" in
    HU.testCase "4" $
    fullParses (parser $ grammar RightAssoc) x
    @?= (,) [Plus (Plus (Var "5") (Var "6")) (Var "7")]
            Report {position = 5, expected = [], unconsumed = []}
  , let x = words "+ 5 + 6 7" in
    HU.testCase "5" $
    fullParses (parser $ grammar RightAssoc) x
    @?= (,) [Plus (Var "5") (Plus (Var "6") (Var "7"))]
            Report {position = 5, expected = [], unconsumed = []}
  , let x = words "+ 5 6" in
    HU.testCase "6" $
    fullParses (parser $ grammar RightAssoc) x
    @?= (,) [Plus (Var "5") (Var "6")]
            Report {position = 3, expected = [], unconsumed = []}
  , let x = words "+ + 5 6 7" in
    HU.testCase "7" $
    fullParses (parser $ grammar NonAssoc) x
    @?= (,) [Plus (Plus (Var "5") (Var "6")) (Var "7")]
            Report {position = 5, expected = [], unconsumed = []}
  , let x = words "+ 5 + 6 7" in
    HU.testCase "8" $
    fullParses (parser $ grammar NonAssoc) x
    @?= (,) [Plus (Var "5") (Plus (Var "6") (Var "7"))]
            Report {position = 5, expected = [], unconsumed = []}
  , let x = words "+ 5 6" in
    HU.testCase "9" $
    fullParses (parser $ grammar NonAssoc) x
    @?= (,) [Plus (Var "5") (Var "6")]
            Report {position = 3, expected = [], unconsumed = []}
  ]

data AST
  = Var String
  | Plus AST AST
  deriving (Eq, Ord, Show)

grammar :: Associativity -> Grammar r m (Prod r m String String AST)
grammar a = mdo
    atomicExpr <- rule $ Var <$> satisfy (/= "+")

    expr <- mixfixExpression
               [[([Just (token "+"), Nothing, Nothing], a)]]
               atomicExpr
               (\x y -> case (x,y) of
                  ([Just "+", Nothing, Nothing], [e1,e2]) -> Plus e1 e2
                  _ -> undefined)

    return expr
