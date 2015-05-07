{-# LANGUAGE RecursiveDo, FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.DeepSeq
import Criterion.Main
import Data.Char
import Data.Maybe
import Text.Earley
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Var String
  deriving (Eq, Ord, Show)

instance NFData Expr where
  rnf (Add a b) = rnf a `seq` rnf b
  rnf (Mul a b) = rnf a `seq` rnf b
  rnf (Var s)   = rnf s

type Token = String

tokenParens :: Bool -> [Token] -> [Token]
tokenParens True s  = ["("] ++ s ++ [")"]
tokenParens False s = s

tokenExpr :: Int -> Expr -> [Token]
tokenExpr _ (Var s) = [s]
tokenExpr d (Add a b) = tokenParens (d > 0) $ tokenExpr 0 a ++ ["+"] ++ tokenExpr 1 b
tokenExpr d (Mul a b) = tokenParens (d > 1) $ tokenExpr 1 a ++ ["*"] ++ tokenExpr 2 b

linearSum :: Int -> Expr
linearSum 1 = Var "x"
linearSum n = Add (linearSum $ n - 1) (Var "x")

treeSum :: Int -> Expr
treeSum 1 = Var "x"
treeSum n = let a = n `div` 2 -- will be at least 1
                b = n - a
            in Add (treeSum a) (treeSum b)

-- Earley parser

expr :: Grammar r String (Prod r String Token Expr)
expr = mdo
  x1 <- rule $ Add <$> x1 <* namedSymbol "+" <*> x2
            <|> x2
            <?> "sum"
  x2 <- rule $ Mul <$> x2 <* namedSymbol "*" <*> x3
            <|> x3
            <?> "product"
  x3 <- rule $ Var <$> (satisfy isIdent <?> "identifier")
            <|> namedSymbol "(" *> x1 <* namedSymbol ")"
  return x1

isIdent :: String -> Bool
isIdent (x:_) = isAlpha x
isIdent _     = False

parseEarley :: [Token] -> Maybe Expr
parseEarley input = listToMaybe (fst (fullParses (parser expr input)))

-- Parsec parsec

type Parsec = Parsec.Parsec [Token] ()

parsecExpr :: Parsec Expr
parsecExpr = mul
  where mul   = foldl1 Mul <$> add `Parsec.sepBy1` t "*"
        add   = foldl1 Add <$> var `Parsec.sepBy1` t "+"
        ident = Parsec.token id pos $ \y -> if isIdent y then Just (Var y) else Nothing
        var   = ident <|> (t "(" *> mul <* t ")")
        t x   = Parsec.token id pos $ \y -> if x == y then Just x else Nothing
        pos   = const (Parsec.initialPos "")

parseParsec :: [Token] -> Maybe Expr
parseParsec =  either (const Nothing) Just . Parsec.parse parsecExpr ""

-- Our benchmark harness.

linearInput :: Int -> (String, [Token])
linearInput size = (show size, tokenExpr 0 $ linearSum size)

treeInput :: Int -> (String, [Token])
treeInput size = (show size, tokenExpr 0 $ treeSum size)

inputBench :: (String, [Token]) -> Benchmark
inputBench (name, input) = bench name $ nf id input

earleyBench :: (String, [Token]) -> Benchmark
earleyBench (name, input) = bench name $ nf parseEarley input

parsecBench :: (String, [Token]) -> Benchmark
parsecBench (name, input) = bench name $ nf parseParsec input

benchSizes :: [Int]
benchSizes = [51, 101, 151, 201]

linearInputs :: [(String, [Token])]
linearInputs = map linearInput benchSizes

treeInputs :: [(String, [Token])]
treeInputs = map treeInput benchSizes

main :: IO ()
main = do
  evaluate (rnf linearInputs)
  evaluate (rnf treeInputs)
  defaultMain
    [ bgroup "inputs" $ map inputBench linearInputs 
    , bgroup "earley" $ map earleyBench linearInputs
    , bgroup "parsec" $ map parsecBench linearInputs
    , bgroup "inputsTree" $ map inputBench treeInputs
    , bgroup "earleyTree" $ map earleyBench treeInputs
    , bgroup "parsecTree" $ map parsecBench treeInputs
    ]

