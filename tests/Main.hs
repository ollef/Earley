module Main where
import Test.Tasty

import qualified Empty
import qualified Expr
import qualified InlineAlts
import qualified Issue11
import qualified Issue14
import qualified Mixfix
import qualified Optional
import qualified ReversedWords
import qualified VeryAmbiguous

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ Empty.tests
  , Expr.tests
  , InlineAlts.tests
  , Issue11.tests
  , Issue14.tests
  , Mixfix.tests
  , Optional.tests
  , ReversedWords.tests
  , VeryAmbiguous.tests
  ]
