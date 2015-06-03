{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit      as HU

import Data.Char
import Control.Applicative
import Text.Earley

main :: IO ()
main = defaultMain tests -- -putStrLn . prettyExpr 0 $ Add (Add (Var "a") (Var "b")) (Add (Var "c") (Var "d")) -- defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "QuickCheck Properties"
  [ QC.testProperty "Expr: parse . pretty = id" $
    \e -> [e] === parseExpr (prettyExpr 0 e)
  , QC.testProperty "Ambiguous Expr: parse . pretty ≈ id" $
    \e -> e `elem` parseAmbiguousExpr (prettyExpr 0 e)
  , QC.testProperty "The empty parser doesn't parse anything" $
    \(input :: String) ->
      allParses (parser (return empty :: forall r. Grammar r () (Prod r () Char ())) input)
      == (,) [] Report { position   = 0
                       , expected   = []
                       , unconsumed = input
                       }
  , QC.testProperty "Many empty parsers parse very little" $
    \(input :: String) ->
      allParses (parser (return $ many empty <* pure "blah" :: forall r. Grammar r () (Prod r () Char [()])) input)
      == (,) [([], 0)] Report { position   = 0
                              , expected   = []
                              , unconsumed = input
                              }
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ HU.testCase "VeryAmbiguous gives the right number of results" $
      length (fst $ fullParses $ parser veryAmbiguous $ replicate 8 'b') @?= 2871
  , HU.testCase "VeryAmbiguous gives the correct report" $
      report (parser veryAmbiguous $ replicate 3 'b') @?=
      Report {position = 3, expected = "s", unconsumed = ""}
  , HU.testCase "Inline alternatives work" $
      let input = "ababbbaaabaa" in
      allParses (parser inlineAlts input) @?= allParses (parser nonInlineAlts input)
  , HU.testCase "Some reversed words" $
      let input = "wordwordstop"
          l     = length input in
      allParses (parser someWords input)
      @?= (,) [(["stop", "drow", "drow"], l)] Report { position   = l
                                                     , expected   = []
                                                     , unconsumed = []
                                                     }
  , HU.testCase "Optional" $
      fullParses (parser (return optional_) "b")
      @?= (,) [(Nothing, 'b')] Report {position = 1, expected = "", unconsumed = ""}
  , HU.testCase "Optional" $
      fullParses (parser (return optional_) "ab")
      @?= (,) [(Just 'a', 'b')] Report {position = 2, expected = "", unconsumed = ""}
  , HU.testCase "Optional using rules" $
      fullParses (parser optionalRule "b")
      @?= (,) [(Nothing, 'b')] Report {position = 1, expected = "", unconsumed = ""}
  , HU.testCase "Optional using rules" $
      fullParses (parser optionalRule "ab")
      @?= (,) [(Just 'a', 'b')] Report {position = 2, expected = "", unconsumed = ""}
  ]

optional_ :: Prod r Char Char (Maybe Char, Char)
optional_ = (,) <$> optional (namedSymbol 'a') <*> namedSymbol 'b'

optionalRule :: Grammar r Char (Prod r Char Char (Maybe Char, Char))
optionalRule = mdo
  test <- rule $ (,) <$> optional (namedSymbol 'a') <*> namedSymbol 'b'
  return test

inlineAlts :: Grammar r Char (Prod r Char Char String)
inlineAlts = mdo
  p <- rule $ pure []
           <|> (:) <$> (namedSymbol 'a' <|> namedSymbol 'b') <*> p
  return p

nonInlineAlts :: Grammar r Char (Prod r Char Char String)
nonInlineAlts = mdo
  ab <- rule $ namedSymbol 'a' <|> namedSymbol 'b'
  p  <- rule $ pure [] <|> (:) <$> ab <*> p
  return p

someWords :: Grammar r () (Prod r () Char [String])
someWords = return $ flip (:) <$> (map reverse <$> some (word "word")) <*> word "stop"

veryAmbiguous :: Grammar r Char (Prod r Char Char ())
veryAmbiguous = mdo
  s <- rule $ () <$ symbol 'b'
           <|> () <$ s <* s
           <|> () <$ s <* s <* s
           <?> 's'
  return s

parseExpr :: String -> [Expr]
parseExpr input = fst (fullParses (parser expr (lexExpr input))) -- We need to annotate types for point-free version

parseAmbiguousExpr :: String -> [Expr]
parseAmbiguousExpr input = fst (fullParses (parser ambiguousExpr (lexExpr input)))

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Var String
  deriving (Eq, Ord, Show)

instance Arbitrary Expr where
  arbitrary = sized arbExpr
    where arbIdent           = Var <$> elements ["a", "b", "c", "x", "y", "z"]
          arbExpr n | n > 0  = oneof [ arbIdent
                                     , Add <$> arbExpr1 <*> arbExpr1
                                     , Mul <$> arbExpr1 <*> arbExpr1
                                     ]
                                     where arbExpr1 = arbExpr (n `div` 2)
          arbExpr _          = arbIdent

  shrink (Var _)    = []
  shrink (Add a b)  = a : b : [ Add a' b | a' <- shrink a ] ++ [ Add a b' | b' <- shrink b ]
  shrink (Mul a b)  = a : b : [ Mul a' b | a' <- shrink a ] ++ [ Mul a b' | b' <- shrink b ]

expr :: Grammar r String (Prod r String String Expr)
expr = mdo
  x1 <- rule $ Add <$> x1 <* namedSymbol "+" <*> x2
            <|> x2
            <?> "sum"
  x2 <- rule $ Mul <$> x2 <* namedSymbol "*" <*> x3
            <|> x3
            <?> "product"
  x3 <- rule $ Var <$> (satisfy ident <?> "identifier")
            <|> namedSymbol "(" *> x1 <* namedSymbol ")"
  return x1
  where
    ident (x:_) = isAlpha x
    ident _     = False

ambiguousExpr :: Grammar r String (Prod r String String Expr)
ambiguousExpr = mdo
  x1 <- rule $ Add <$> x1 <* namedSymbol "+" <*> x1
            <|> x2
            <?> "sum"
  x2 <- rule $ Mul <$> x2 <* namedSymbol "*" <*> x2
            <|> x3
            <?> "product"
  x3 <- rule $ Var <$> (satisfy ident <?> "identifier")
            <|> namedSymbol "(" *> x1 <* namedSymbol ")"
  return x1
  where
    ident (x:_) = isAlpha x
    ident _     = False

prettyParens :: Bool -> String -> String
prettyParens True s  = "(" ++ s ++ ")"
prettyParens False s = s

prettyExpr :: Int -> Expr -> String
prettyExpr _ (Var s) = s
prettyExpr d (Add a b) = prettyParens (d > 0) $ prettyExpr 0 a ++ " + " ++ prettyExpr 1 b
prettyExpr d (Mul a b) = prettyParens (d > 1) $ prettyExpr 1 a ++ " * " ++ prettyExpr 2 b

-- @words@ like lexer, but consider parentheses as separate tokens
lexExpr :: String -> [String]
lexExpr ""        = []
lexExpr ('(' : s) = "(" : lexExpr s
lexExpr (')' : s) = ")" : lexExpr s
lexExpr (c : s)
  | isSpace c     = lexExpr s
  | otherwise     = let (tok, rest) = span p (c : s)
                    in tok : lexExpr rest
  where p x       = not (x == '(' || x == ')' || isSpace x)
