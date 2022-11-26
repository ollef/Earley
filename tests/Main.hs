module Main where
import Test.Tasty

import qualified Constraint
import qualified Empty
import qualified Expr
import qualified Generator
import qualified InlineAlts
import qualified Issue11
import qualified Issue14
import qualified Lambda
import qualified Mixfix
import qualified Optional
import qualified ReversedWords
import qualified UnbalancedPars
import qualified VeryAmbiguous

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ Constraint.tests
  , Empty.tests
  , Expr.tests
  , Generator.tests
  , InlineAlts.tests
  , Issue11.tests
  , Issue14.tests
  , Lambda.tests
  , Mixfix.tests
  , Optional.tests
  , ReversedWords.tests
  , UnbalancedPars.tests
  , VeryAmbiguous.tests
  ]
