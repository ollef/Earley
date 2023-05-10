module Constraint where
import Control.Applicative
import Data.List
import Data.Set(fromList)

import Test.Tasty
import Test.Tasty.HUnit as HU

import Text.Earley

oneToken :: Grammar r m (Prod r m () t t)
oneToken = rule anyToken

someTokens :: Grammar r m (Prod r m () t [t])
someTokens = rule (some anyToken)

tests :: TestTree
tests = testGroup "New features"
  [ HU.testCase "anyToken1" $
      let input = "hello"
          l     = length input in
      allParses (parser oneToken) input
      @?= (,) [('h', 1)] Report { position   = 1
                                , expected   = []
                                , unconsumed = drop 1 input
                                }
  , HU.testCase "anyToken2" $
      allParses (parser oneToken) ""
      @?= (,) [] Report { position = 0
                        , expected   = []
                        , unconsumed = ""
                        }
  , HU.testCase "anyToken3" $
      let input = "hello"
          l     = length input in
      allParses (parser someTokens) input
      @?= (,) [(init, length init) | init <- inits input, not (null init) ]
              Report { position   = l
                     , expected   = []
                     , unconsumed = []
                     }
  , HU.testCase "constraint" $
      matches noRepeats "salut"
      @?= True
  , HU.testCase "constraint2" $
      matches noRepeats "hello"
      @?= False
  , HU.testCase "constraint3" $
      fromList (map fst $ exactly 2 $ generator noRepeats "abcd")
      @?= fromList [[x, y] | x <- "abcd", y <- "abcd", x /= y]
  ]

noRepeats = rule $
  constraint (\x -> length x == length (fromList x)) $
    many anyToken
