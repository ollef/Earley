module ReversedWords where
import Control.Applicative
import Test.Tasty
import Test.Tasty.HUnit as HU

import Text.Earley

someWords :: Grammar r m (Prod r m () Char [String])
someWords = return $ flip (:) <$> (map reverse <$> some (list "word")) <*> list "stop"

tests :: TestTree
tests = testGroup "Reversed words"
  [ HU.testCase "Parse" $
      let input = "wordwordstop"
          l     = length input in
      allParses (parser someWords) input
      @?= (,) [(["stop", "drow", "drow"], l)] Report { position   = l
                                                     , expected   = []
                                                     , unconsumed = []
                                                     }
  , HU.testCase "Generate" $
    upTo 16 (generator someWords "stopwrd")
    @?=
    [ (["stop", "drow"], "wordstop")
    , (["stop", "drow", "drow"], "wordwordstop")
    , (["stop","drow","drow","drow"],"wordwordwordstop")
    ]
  ]
