module ReversedWords where
import Control.Applicative
import Test.Tasty
import Test.Tasty.HUnit as HU

import Text.Earley

someWords :: Grammar r (Prod r () Char [String])
someWords = return $ flip (:) <$> (map reverse <$> some (word "word")) <*> word "stop"

tests :: TestTree
tests = testGroup "Unit Tests"
  [ HU.testCase "Some reversed words" $
      let input = "wordwordstop"
          l     = length input in
      allParses (parser someWords) input
      @?= (,) [(["stop", "drow", "drow"], l)] Report { position   = l
                                                     , expected   = []
                                                     , unconsumed = []
                                                     }
  ]
