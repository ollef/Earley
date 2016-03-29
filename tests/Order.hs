{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
module Order where
import Control.Applicative
import Data.List(sort)
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Text.Earley

tests :: TestTree
tests = testGroup "Result order"
  [ QC.testProperty "Simple results are ordered" $
    \(input :: [Bool]) ->
    let results = fst $ fullParses (parser simple) (take 10 input)
     in results == sort results
  , QC.testProperty "Complicated results are ordered" $
    \(input :: [Bool]) ->
    let results = fst $ fullParses (parser complicated) (take 10 input)
     in results == sort results
  ]

simple :: Grammar r (Prod r e Bool [Int])
simple = mdo
  x <- rule $  0 <$ token True
           <|> 1 <$ token False
  y <- rule $  2 <$ token True
           <|> 3 <$ token False
  xs <- rule $ (:) <$> (x <|> y) <*> xs
            <|> pure []
  return xs

simple2 :: Grammar r (Prod r e Bool [Int])
simple2 = mdo
  let onetwo = pure 1 <$ token True <|> pure 2 <$ token True

  xs <- rule onetwo
  ys <- rule $ (\a b c -> concat [a,b,c]) <$> xs <*> onetwo <*> xs
  return ys

simple3 :: Grammar r (Prod r e Bool [Int])
simple3 = mdo
  x <- rule $  0 <$ token True
  y <- rule $  1 <$ token True
  return $ (\a b -> [a, b]) <$> (x <|> y) <*> (x <|> y)

simple4 :: Grammar r (Prod r e Bool [Int])
simple4 = mdo
  let x = 0 <$ token True
  let y = 1 <$ token True
  rule $ (\a b -> [a, b]) <$> (x <|> y) <*> (x <|> y)

simple5 :: Grammar r (Prod r e Bool [Int])
simple5 = mdo
  let tt n = n <$ token True
      f a b = [a,b]
  return $ f <$> tt 1 <*> tt 3 <|> f <$> tt 2 <*> tt 4

simple6 :: Grammar r (Prod r e Bool [Int])
simple6 = return $ [1] <$ token True <|> [2] <$ token True


complicated :: Grammar r (Prod r e Bool [Int])
complicated = mdo
  p1 <- rule =<< simple
  p2 <- rule $ pure [4] <|> pure [5]
  return $ concat <$> sequenceA [p1, p2, p1, p2]
