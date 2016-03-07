{-# LANGUAGE ScopedTypeVariables #-}
module Empty where
import Control.Applicative
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Text.Earley

tests :: TestTree
tests = testGroup "Empty productions"
  [ QC.testProperty "The empty production doesn't parse anything" $
    \(input :: String) ->
      allParses (parser (return empty :: forall r. Grammar r (Prod r () Char ()))) input
      == (,) [] Report { position   = 0
                       , expected   = []
                       , unconsumed = input
                       }
  , QC.testProperty "Many empty productions parse very little" $
    \(input :: String) ->
      allParses (parser (return $ many empty <* pure "blah" :: forall r. Grammar r (Prod r () Char [()]))) input
      == (,) [([], 0)] Report { position   = 0
                              , expected   = []
                              , unconsumed = input
                              }
  ]
