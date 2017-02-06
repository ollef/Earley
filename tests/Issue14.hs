module Issue14 where
import Control.Applicative
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Text.Earley

tests :: TestTree
tests = testGroup "Issue 14"
  [ QC.testProperty "The same rule in alternatives gives many results" $
    \x -> fullParses (parser (issue14 x)) ""
    == (,) (replicate (issue14Length x) ())
           Report { position = 0, expected = [], unconsumed = [] }
  ]

data Issue14 a
  = Pure a
  | Alt (Issue14 a) (Issue14 a)
  | Ap (Issue14 a) (Issue14 a)
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Issue14 a) where
  arbitrary = sized arbTree
    where arbTree n | n > 0  = oneof [ Pure <$> arbitrary
                                     , Alt <$> arbTree1 <*> arbTree1
                                     , Ap <$> arbTree1 <*> arbTree1
                                     ]
                                     where arbTree1 = arbTree (n `div` 2)
          arbTree _          = Pure <$> arbitrary

  shrink (Pure a)  = Pure <$> shrink a
  shrink (Alt a b) = a : b : [Alt a' b | a' <- shrink a] ++ [Alt a b' | b' <- shrink b]
  shrink (Ap a b)  = a : b : [Ap a' b | a' <- shrink a] ++ [Ap a b' | b' <- shrink b]

issue14Length :: Issue14 () -> Int
issue14Length (Pure ()) = 1
issue14Length (Alt a b) = issue14Length a + issue14Length b
issue14Length (Ap a b)  = issue14Length a * issue14Length b

issue14 :: Issue14 () -> Grammar r (Prod r () Char ())
issue14 tree = do
  emptyRule <- rule $ pure ()
  let x = go emptyRule tree
  return x
  where
    go x (Pure ())   = x
    go x (Alt b1 b2) = go x b1 <|> go x b2
    go x (Ap b1 b2)  = go x b1 <* go x b2
