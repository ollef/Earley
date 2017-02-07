{-# LANGUAGE ScopedTypeVariables #-}
module Empty where
import Control.Applicative
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC

import Text.Earley

tests :: TestTree
tests = testGroup "Empty productions"
  [ QC.testProperty "The empty production doesn't parse anything" $
    \(input :: String) ->
      allParses (parser emptyGrammar) input
      == (,) [] Report { position   = 0
                       , expected   = []
                       , unconsumed = input
                       }
  , HU.testCase "The empty production doesn't generate anything" $
      language (generator emptyGrammar "abc") @?= []
  , QC.testProperty "Many empty productions parse very little" $
    \(input :: String) ->
      allParses (parser manyEmpty) input
      == (,) [([], 0)] Report { position   = 0
                              , expected   = []
                              , unconsumed = input
                              }
  , HU.testCase "Many empty productions generate very little" $
      language (generator manyEmpty "blahc") @?= [([], "")]
  ]

emptyGrammar :: Grammar r (Prod r () Char ())
emptyGrammar = return empty

manyEmpty :: Grammar r (Prod r () Char [()])
manyEmpty = return $ many empty <* pure "blah"
