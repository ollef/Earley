{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
module InlineAlts where
import Control.Applicative
import Test.Tasty
import Test.Tasty.HUnit as HU

import Text.Earley

tests :: TestTree
tests = testGroup "Inline alternatives"
  [ HU.testCase "They work when parsed" $
      let input = "ababbbaaabaa" in
      allParses (parser inlineAlts) input @?= allParses (parser nonInlineAlts) input
  , HU.testCase "They work when generated" $
      take 1000 (language $ generator inlineAlts "ab") @?=
      take 1000 (language $ generator nonInlineAlts "ab")
  ]

inlineAlts :: Grammar r (Prod r Char Char String)
inlineAlts = mdo
  p <- rule $ pure []
           <|> (:) <$> (namedToken 'a' <|> namedToken 'b') <*> p
  return p

nonInlineAlts :: Grammar r (Prod r Char Char String)
nonInlineAlts = mdo
  ab <- rule $ namedToken 'a' <|> namedToken 'b'
  p  <- rule $ pure [] <|> (:) <$> ab <*> p
  return p
