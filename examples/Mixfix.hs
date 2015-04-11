{-# LANGUAGE RecursiveDo #-}
import Control.Applicative hiding (some)
import Control.Arrow(first)
import Data.Foldable(foldrM)
import System.Environment
import Text.Earley
import Data.List
import qualified Data.Set as S

type Ident = String

data IdentPart = Ident Ident | Hole
  deriving Show

type HoleyIdent = [IdentPart]

holey :: Ident -> HoleyIdent
holey ""       = []
holey ('_':xs) = Hole    : holey xs
holey xs       = Ident i : holey rest
  where (i, rest) = span (/= '_') xs 

data Assoc = LeftAssoc | RightAssoc | NonAssoc
  deriving (Eq, Show)

data Expr = V HoleyIdent | App Expr [Expr]
  deriving Show

grammar :: [[(HoleyIdent, Assoc)]] -> Grammar r (Prod r String Expr)
grammar table = mdo
  let ident = (V . (:[]) . Ident) <$> satisfy (`S.notMember` mixfixParts)
  expr  <- foldrM ($) ident (normalApp expr : levels expr)
  return expr
  where
    mixfixParts = S.fromList [s | xs <- table, (ys, _) <- xs, Ident s <- ys]
    normalApp expr next = do
      args <- some next
      rule [ App <$> expr <*> args
           , next
           ]
    levels expr = map (level expr) table
    level expr idents next = mdo
      same <- rule $ next : map (mixfixIdent same) idents
      return same
      where
        mixfixIdent same (ps, a) = App (V ps) <$> go ps
          where
            go ps' = case ps' of
              [Ident s]         -> []    <$  symbol s
              Hole:rest         -> (:)   <$> (if a == RightAssoc then next else same) <*> go rest
              [Ident s, Hole]   -> (:[]) <$  symbol s <*> (if a == LeftAssoc then next else same)
              Ident s:Hole:rest -> (:)   <$  symbol s <*> expr <*> go rest
              _                 -> error "invalid identifier"

identTable :: [[(HoleyIdent, Assoc)]]
identTable = (map . map) (first holey)
  [ [("_->_",          RightAssoc)]
  , [("_,_",           NonAssoc)]
  , [("if_then_else_", RightAssoc)]
  , [("_|-_:_",        NonAssoc)]
  , [("_+_",           LeftAssoc)]
  , [("_*_",           LeftAssoc)]
  , [("(_)",           NonAssoc)
    ,("[_]",           NonAssoc)
    ,("<_>",           NonAssoc)
    ]
  ]

pretty :: Expr -> String
pretty (V ps) = concatMap go ps
  where
    go Hole      = "_"
    go (Ident s) = s
pretty (App e es) = "(" ++ pretty e ++ " " ++ unwords (map pretty es) ++ ")"

tokenize :: String -> [Ident]
tokenize ""        = []
tokenize (' ':xs)  = tokenize xs
tokenize ('\n':xs) = tokenize xs
tokenize (x:xs)
  | x `S.member` special = [x] : tokenize xs
  | otherwise            = (x:as) : tokenize bs
  where
    (as, bs) = span (`S.notMember` special) xs
    special = S.fromList "()[], \n"

main :: IO ()
main = do
  x:_ <- getArgs
  print $ map pretty $ fullParses $ parser (grammar identTable) (tokenize x)
