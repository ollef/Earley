{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
import Control.Applicative
import Data.Char
import Test.Tasty
import Test.Tasty.HUnit      as HU
import Test.Tasty.QuickCheck as QC

import Text.Earley
import Text.Earley.Mixfix

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
      allParses (parser (return empty :: forall r. Grammar r (Prod r () Char ()))) input
      == (,) [] Report { position   = 0
                       , expected   = []
                       , unconsumed = input
                       }
  , QC.testProperty "Many empty parsers parse very little" $
    \(input :: String) ->
      allParses (parser (return $ many empty <* pure "blah" :: forall r. Grammar r (Prod r () Char [()]))) input
      == (,) [([], 0)] Report { position   = 0
                              , expected   = []
                              , unconsumed = input
                              }
  , QC.testProperty "The same rule in alternatives gives many results (issue #14)" $
    \x -> fullParses (parser (issue14 x)) ""
    == (,) (replicate (issue14Length x) ())
           Report { position = 0, expected = [], unconsumed = [] }
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ HU.testCase "VeryAmbiguous gives the right number of results" $
      length (fst $ fullParses (parser veryAmbiguous) $ replicate 8 'b') @?= 2871
  , HU.testCase "VeryAmbiguous gives the correct report" $
      report (parser veryAmbiguous) (replicate 3 'b') @?=
      Report {position = 3, expected = "s", unconsumed = ""}
  , HU.testCase "Inline alternatives work" $
      let input = "ababbbaaabaa" in
      allParses (parser inlineAlts) input @?= allParses (parser nonInlineAlts) input
  , HU.testCase "Some reversed words" $
      let input = "wordwordstop"
          l     = length input in
      allParses (parser someWords) input
      @?= (,) [(["stop", "drow", "drow"], l)] Report { position   = l
                                                     , expected   = []
                                                     , unconsumed = []
                                                     }
  , HU.testCase "Optional Nothing" $
      fullParses (parser $ return optional_) "b"
      @?= (,) [(Nothing, 'b')] Report {position = 1, expected = "", unconsumed = ""}
  , HU.testCase "Optional Just" $
      fullParses (parser $ return optional_) "ab"
      @?= (,) [(Just 'a', 'b')] Report {position = 2, expected = "", unconsumed = ""}
  , HU.testCase "Optional using rules Nothing" $
      fullParses (parser $ optionalRule) "b"
      @?= (,) [(Nothing, 'b')] Report {position = 1, expected = "", unconsumed = ""}
  , HU.testCase "Optional using rules Just" $
      fullParses (parser $ optionalRule) "ab"
      @?= (,) [(Just 'a', 'b')] Report {position = 2, expected = "", unconsumed = ""}
  , HU.testCase "Optional without continuation Nothing" $
      fullParses (parser $ return $ optional $ namedSymbol 'a') ""
      @?= (,) [Nothing] Report {position = 0, expected = "a", unconsumed = ""}
  , HU.testCase "Optional without continuation Just" $
      fullParses (parser $ return $ optional $ namedSymbol 'a') "a"
      @?= (,) [Just 'a'] Report {position = 1, expected = "", unconsumed = ""}
  , HU.testCase "Optional using rules without continuation Nothing" $
      fullParses (parser $ rule $ optional $ namedSymbol 'a') ""
      @?= (,) [Nothing] Report {position = 0, expected = "a", unconsumed = ""}
  , HU.testCase "Optional using rules without continuation Just" $
      fullParses (parser $ rule $ optional $ namedSymbol 'a') "a"
      @?= (,) [Just 'a'] Report {position = 1, expected = "", unconsumed = ""}

  , HU.testCase "Mixfix 1" $
      let x = Ident [Just "x"] in
      fullParses (parser mixfixGrammar) (words "if x then x else x")
      @?= (,) [App ifthenelse [x, x, x]] Report {position = 6, expected = [], unconsumed = []}
  , HU.testCase "Mixfix 2" $
      let x = Ident [Just "x"] in
      fullParses (parser mixfixGrammar) (words "prefix x postfix")
      @?= (,) [App prefix [App postfix [x]]] Report {position = 3, expected = [], unconsumed = []}
  , HU.testCase "Mixfix 3" $
      let x = Ident [Just "x"] in
      fullParses (parser mixfixGrammar) (words "x infix1 x infix2 x")
      @?= (,) [App infix1 [x, App infix2 [x, x]]] Report {position = 5, expected = [], unconsumed = []}
  , HU.testCase "Mixfix 4" $
      let x = Ident [Just "x"] in
      fullParses (parser mixfixGrammar) (words "[ x ]")
      @?= (,) [App closed [x]] Report {position = 3, expected = [], unconsumed = []}

  , let x = words "+ + 5 6 7" in
    HU.testCase "Mixfix issue #11 1" $
    fullParses (parser $ issue11 LeftAssoc) x
    @?= (,) [] Report {position = 1, expected = [], unconsumed = drop 1 x}
  , let x = words "+ 5 + 6 7" in
    HU.testCase "Mixfix issue #11 2" $
    fullParses (parser $ issue11 LeftAssoc) x
    @?= (,) [] Report {position = 2, expected = [], unconsumed = drop 2 x}
  , let x = words "+ 5 6" in
    HU.testCase "Mixfix issue #11 3" $
    fullParses (parser $ issue11 LeftAssoc) x
    @?= (,) [Plus11 (Var11 "5") (Var11 "6")]
            Report {position = 3, expected = [], unconsumed = []}
  , let x = words "+ + 5 6 7" in
    HU.testCase "Mixfix issue #11 4" $
    fullParses (parser $ issue11 RightAssoc) x
    @?= (,) [Plus11 (Plus11 (Var11 "5") (Var11 "6")) (Var11 "7")]
            Report {position = 5, expected = [], unconsumed = []}
  , let x = words "+ 5 + 6 7" in
    HU.testCase "Mixfix issue #11 5" $
    fullParses (parser $ issue11 RightAssoc) x
    @?= (,) [Plus11 (Var11 "5") (Plus11 (Var11 "6") (Var11 "7"))]
            Report {position = 5, expected = [], unconsumed = []}
  , let x = words "+ 5 6" in
    HU.testCase "Mixfix issue #11 6" $
    fullParses (parser $ issue11 RightAssoc) x
    @?= (,) [Plus11 (Var11 "5") (Var11 "6")]
            Report {position = 3, expected = [], unconsumed = []}
  , let x = words "+ + 5 6 7" in
    HU.testCase "Mixfix issue #11 7" $
    fullParses (parser $ issue11 NonAssoc) x
    @?= (,) [Plus11 (Plus11 (Var11 "5") (Var11 "6")) (Var11 "7")]
            Report {position = 5, expected = [], unconsumed = []}
  , let x = words "+ 5 + 6 7" in
    HU.testCase "Mixfix issue #11 8" $
    fullParses (parser $ issue11 NonAssoc) x
    @?= (,) [Plus11 (Var11 "5") (Plus11 (Var11 "6") (Var11 "7"))]
            Report {position = 5, expected = [], unconsumed = []}
  , let x = words "+ 5 6" in
    HU.testCase "Mixfix issue #11 9" $
    fullParses (parser $ issue11 NonAssoc) x
    @?= (,) [Plus11 (Var11 "5") (Var11 "6")]
            Report {position = 3, expected = [], unconsumed = []}
  ]

optional_ :: Prod r Char Char (Maybe Char, Char)
optional_ = (,) <$> optional (namedSymbol 'a') <*> namedSymbol 'b'

optionalRule :: Grammar r (Prod r Char Char (Maybe Char, Char))
optionalRule = mdo
  test <- rule $ (,) <$> optional (namedSymbol 'a') <*> namedSymbol 'b'
  return test

inlineAlts :: Grammar r (Prod r Char Char String)
inlineAlts = mdo
  p <- rule $ pure []
           <|> (:) <$> (namedSymbol 'a' <|> namedSymbol 'b') <*> p
  return p

nonInlineAlts :: Grammar r (Prod r Char Char String)
nonInlineAlts = mdo
  ab <- rule $ namedSymbol 'a' <|> namedSymbol 'b'
  p  <- rule $ pure [] <|> (:) <$> ab <*> p
  return p

someWords :: Grammar r (Prod r () Char [String])
someWords = return $ flip (:) <$> (map reverse <$> some (word "word")) <*> word "stop"

veryAmbiguous :: Grammar r (Prod r Char Char ())
veryAmbiguous = mdo
  s <- rule $ () <$ symbol 'b'
           <|> () <$ s <* s
           <|> () <$ s <* s <* s
           <?> 's'
  return s

parseExpr :: String -> [Expr]
parseExpr input = fst (fullParses (parser expr) (lexExpr input)) -- We need to annotate types for point-free version

parseAmbiguousExpr :: String -> [Expr]
parseAmbiguousExpr input = fst (fullParses (parser ambiguousExpr) (lexExpr input))

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

expr :: Grammar r (Prod r String String Expr)
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

ambiguousExpr :: Grammar r (Prod r String String Expr)
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

data MixfixExpr = Ident (Holey String) | App (Holey String) [MixfixExpr]
  deriving (Eq, Show)

mixfixGrammar :: Grammar r (Prod r String String MixfixExpr)
mixfixGrammar = mixfixExpression table
                                 (Ident . pure . Just <$> namedSymbol "x")
                                 App
  where
    hident = map (fmap symbol)
    table =
      [ [(hident ifthenelse, RightAssoc)]
      , [(hident prefix, RightAssoc)]
      , [(hident postfix, LeftAssoc)]
      , [(hident infix1, LeftAssoc)]
      , [(hident infix2, RightAssoc)]
      , [(hident closed, NonAssoc)]
      ]

ifthenelse, prefix, postfix, infix1, infix2, closed :: Holey String
ifthenelse = [Just "if", Nothing, Just "then", Nothing, Just "else", Nothing]
prefix = [Just "prefix", Nothing]
postfix = [Nothing, Just "postfix"]
infix1 = [Nothing, Just "infix1", Nothing]
infix2 = [Nothing, Just "infix2", Nothing]
closed = [Just "[", Nothing, Just "]"]

-- Adapted from issue #11
data Mixfix11
  = Var11 String
  | Plus11 Mixfix11 Mixfix11
  deriving (Eq, Ord, Show)

issue11 :: Associativity -> Grammar r (Prod r String String Mixfix11)
issue11 a = mdo
    atomicExpr <- rule $ Var11 <$> satisfy (/= "+")

    expr <- mixfixExpression
               [[([Just (symbol "+"), Nothing, Nothing], a)]]
               atomicExpr
               (\x y -> case (x,y) of
                  ([Just "+", Nothing, Nothing], [e1,e2]) -> Plus11 e1 e2
                  _ -> undefined)

    return expr

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
issue14Length (Alt a b) = ((+) $! issue14Length a) $! issue14Length b
issue14Length (Ap a b)  = ((*) $! issue14Length a) $! issue14Length b

issue14 :: Issue14 () -> Grammar r (Prod r () Char ())
issue14 tree = do
  emptyRule <- rule $ pure ()
  let x = go emptyRule tree
  return x
  where
    go x (Pure ())   = x
    go x (Alt b1 b2) = go x b1 <|> go x b2
    go x (Ap b1 b2)  = go x b1 <* go x b2

