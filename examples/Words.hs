{-# LANGUAGE RecursiveDo #-}
import Data.Char
import System.Environment

import Text.Earley as E

grammar :: Grammar r (Prod r Char [String])
grammar = mdo
  whitespace <- rule
    [() <$ whitespace1
    ,pure ()
    ]

  whitespace1 <- rule
    [() <$ satisfy isSpace <* whitespace]

  ident <- rule
    [(:) <$> satisfy isAlpha    <*> identCont]

  identCont <- E.many $ satisfy isAlphaNum

  expr <- rule
    [(:)   <$> ident <* whitespace1 <*> expr
    ,(:[]) <$> ident <* whitespace
    ]

  return expr

main :: IO ()
main = do
  x:_ <- getArgs
  print $ length $ fullParses $ parser grammar $ concat $ replicate (read x) "aaa "
