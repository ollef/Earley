{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
module Optional where
import Control.Applicative
import Test.Tasty
import Test.Tasty.HUnit as HU

import Text.Earley

tests :: TestTree
tests = testGroup "Optional"
  [ HU.testCase "Nothing" $
      fullParses (parser $ return optional_) "b"
      @?= (,) [(Nothing, 'b')] Report {position = 1, expected = "", unconsumed = ""}
  , HU.testCase "Just" $
      fullParses (parser $ return optional_) "ab"
      @?= (,) [(Just 'a', 'b')] Report {position = 2, expected = "", unconsumed = ""}
  , HU.testCase "Using rules Nothing" $
      fullParses (parser optionalRule) "b"
      @?= (,) [(Nothing, 'b')] Report {position = 1, expected = "", unconsumed = ""}
  , HU.testCase "Using rules Just" $
      fullParses (parser optionalRule) "ab"
      @?= (,) [(Just 'a', 'b')] Report {position = 2, expected = "", unconsumed = ""}
  , HU.testCase "Without continuation Nothing" $
      fullParses (parser $ return $ optional $ namedToken 'a') ""
      @?= (,) [Nothing] Report {position = 0, expected = "a", unconsumed = ""}
  , HU.testCase "Without continuation Just" $
      fullParses (parser $ return $ optional $ namedToken 'a') "a"
      @?= (,) [Just 'a'] Report {position = 1, expected = "", unconsumed = ""}
  , HU.testCase "Using rules without continuation Nothing" $
      fullParses (parser $ rule $ optional $ namedToken 'a') ""
      @?= (,) [Nothing] Report {position = 0, expected = "a", unconsumed = ""}
  , HU.testCase "Using rules without continuation Just" $
      fullParses (parser $ rule $ optional $ namedToken 'a') "a"
      @?= (,) [Just 'a'] Report {position = 1, expected = "", unconsumed = ""}
  ]

optional_ :: Prod r Char Char (Maybe Char, Char)
optional_ = (,) <$> optional (namedToken 'a') <*> namedToken 'b'

optionalRule :: Grammar r (Prod r Char Char (Maybe Char, Char))
optionalRule = mdo
  test <- rule $ (,) <$> optional (namedToken 'a') <*> namedToken 'b'
  return test
