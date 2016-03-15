{-# LANGUAGE CPP, RecursiveDo #-}
import Control.Applicative
import Control.Arrow(first)
import Data.Maybe
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
import System.Environment
import Text.Earley
import Text.Earley.Mixfix
import qualified Data.HashSet as HS

holey :: String -> Holey String
holey ""       = []
holey ('_':xs) = Nothing : holey xs
holey xs       = Just i : holey rest
  where (i, rest) = span (/= '_') xs

data Expr = V (Holey String) | App Expr [Expr]
  deriving Show

identTable :: [[(Holey String, Associativity)]]
identTable = (map . map) (first holey)
  [ [("_->_",          RightAssoc)]
  , [("_,_",           NonAssoc)]
  , [("if_then_else_", RightAssoc)]
  , [("_|-_:_",        NonAssoc)]
  , [("_+_",           LeftAssoc)]
  , [("_*_",           LeftAssoc)]
  ]

grammar :: Grammar r (Prod r String String Expr)
grammar = mdo
  ident     <- rule $ (V . pure . Just) <$> satisfy (not . (`HS.member` mixfixParts))
                   <?> "identifier"
  atom      <- rule $ ident
                   <|> namedToken "(" *> expr <* namedToken ")"
  normalApp <- rule $ atom
                   <|> App <$> atom <*> some atom
  expr      <- mixfixExpression table normalApp (App . V)
  return expr
  where
    table = map (map $ first $ map $ fmap namedToken) identTable
    mixfixParts = HS.fromList [s | xs <- identTable , (ys, _) <- xs
                                 , Just s <- ys]
               `mappend` HS.fromList ["(", ")"]

pretty :: Expr -> String
pretty (V ps) = concatMap (fromMaybe "_") ps
pretty (App e es) = "(" ++ pretty e ++ " " ++ unwords (map pretty es) ++ ")"

tokenize :: String -> [String]
tokenize ""        = []
tokenize (' ':xs)  = tokenize xs
tokenize ('\n':xs) = tokenize xs
tokenize (x:xs)
  | x `HS.member` special = [x] : tokenize xs
  | otherwise             = (x:as) : tokenize bs
  where
    (as, bs) = break (`HS.member` special) xs
    special = HS.fromList "(), \n"

main :: IO ()
main = do
  x:_ <- getArgs
  print $ first (map pretty) $ fullParses (parser grammar) $ tokenize x
