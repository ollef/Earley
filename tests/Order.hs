{-# LANGUAGE ImpredicativeTypes, RecursiveDo, ScopedTypeVariables #-}
module Order where
import Control.Applicative
import Data.List(sort)
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Text.Earley

tests :: TestTree
tests = testGroup "Results are ordered"
  [ QC.testProperty name $
    \(input :: [Bool]) ->
    let results = fst $ fullParses (parser p) (take 10 input)
     in results == sort results
  | (name, p) <- xs
  ]
  where
    xs :: [(String, forall r. Grammar r (Prod r e Bool [Int]))]
    xs =
      [ ("Alternatives", alternatives)
      , ("Alternatives 2", alternatives2)
      , ("One rule", oneRule)
      , ("One rule 2", oneRule2)
      , ("Two rules", twoRules)
      , ("Two rules 2", twoRules2)
      , ("Three rules", threeRules)
      , ("Three rules 2", threeRules2)
      , ("Ambiguous", ambiguous)
      , ("Ambiguous 2", ambiguous2)
      , ("Nulls", nulls)
      , ("Nulls 2", nulls2)
      , ("Nulls 3", nulls3)
      -- TODO many
      -- TODO allParses
      -- TODO named
      ]

alternatives :: Grammar r (Prod r e Bool [Int])
alternatives = mdo
  let x =  0 <$ token False
       <|> 1 <$ token False
       <|> 2 <$ token True
       <|> 3 <$ token True
  xs <- rule $ (:) <$> x <*> xs
            <|> pure []
  return xs

alternatives2 :: Grammar r (Prod r e Bool [Int])
alternatives2 = mdo
  let x =   0 <$ token False
        <|> 1 <$ token True
        <|> 2 <$ token False
        <|> 3 <$ token True
  xs <- rule $ (:) <$> x <*> xs
            <|> pure []
  return xs

oneRule :: Grammar r (Prod r e Bool [Int])
oneRule = mdo
  x <- rule $  0 <$ token False
           <|> 1 <$ token False
           <|> 2 <$ token True
           <|> 3 <$ token True
  xs <- rule $ (:) <$> x <*> xs
            <|> pure []
  return xs

oneRule2 :: Grammar r (Prod r e Bool [Int])
oneRule2 = mdo
  x <- rule $  0 <$ token False
           <|> 1 <$ token True
           <|> 2 <$ token False
           <|> 3 <$ token True
  xs <- rule $ (:) <$> x <*> xs
            <|> pure []
  return xs

twoRules :: Grammar r (Prod r e Bool [Int])
twoRules = mdo
  x <- rule $  0 <$ token False
           <|> 1 <$ token False
  y <- rule $  2 <$ token True
           <|> 3 <$ token True
  xs <- rule $ (:) <$> (x <|> y) <*> xs
            <|> pure []
  return xs

twoRules2 :: Grammar r (Prod r e Bool [Int])
twoRules2 = mdo
  x <- rule $  0 <$ token False
           <|> 1 <$ token True
  y <- rule $  2 <$ token False
           <|> 3 <$ token True
  xs <- rule $ (:) <$> (x <|> y) <*> xs
            <|> pure []
  return xs

threeRules :: Grammar r (Prod r e Bool [Int])
threeRules = mdo
  x <- rule $  0 <$ token False
           <|> 1 <$ token False
  y <- rule $  2 <$ token True
           <|> 3 <$ token True
  xy <- rule $ x <|> y
  xs <- rule $ (:) <$> xy <*> xs
            <|> pure []
  return xs

threeRules2 :: Grammar r (Prod r e Bool [Int])
threeRules2 = mdo
  x <- rule $  0 <$ token False
           <|> 1 <$ token True
  y <- rule $  2 <$ token False
           <|> 3 <$ token True
  xy <- rule $ x <|> y
  xs <- rule $ (:) <$> xy <*> xs
            <|> pure []
  return xs

ambiguous :: Grammar r (Prod r e Bool [Int])
ambiguous = mdo
  x <- rule $  [0] <$ token False
           <|> [1] <$ token True
  let xx = (2 :) <$> x <|> (3 :) <$> x
  xs <- rule $ (++) <$> xx <*> xs
            <|> pure []
  return xs

ambiguous2 :: Grammar r (Prod r e Bool [Int])
ambiguous2 = mdo
  x <- rule $  [0] <$ token False
           <|> [1] <$ token True
  xx <- rule $ (2 :) <$> x <|> (3 :) <$> x
  xs <- rule $ (++) <$> xx <*> xs
            <|> pure []
  return xs

nulls :: Grammar r (Prod r e Bool [Int])
nulls = return $ pure [0] <|> pure [1] <|> pure [2]

nulls2 :: Grammar r (Prod r e Bool [Int])
nulls2 = rule $ pure [0] <|> pure [1] <|> pure [2]

nulls3 :: Grammar r (Prod r e Bool [Int])
nulls3 = rule =<< rule (pure [0] <|> pure [1] <|> pure [2])
