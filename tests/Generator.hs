module Generator where
import Control.Applicative
import Test.Tasty
import Test.Tasty.HUnit as HU

import Text.Earley

tests :: TestTree
tests = testGroup "Lambda"
  [ HU.testCase "Generate exactly 0" $
      exactly 0 (generator (pure $ pure ()) "") @?= [((), [])]
  , HU.testCase "Generate upTo 0" $
      upTo 0 (generator (pure $ pure ()) "") @?= [((), [])]
  , HU.testCase "Generate exactly 1" $
      exactly 1 (generator (pure $ pure ()) "") @?= []
  , HU.testCase "Generate upTo 1" $
      upTo 1 (generator (pure $ pure ()) "") @?= [((), [])]
  ]
