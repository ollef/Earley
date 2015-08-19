{-# LANGUAGE RecursiveDo #-}
import Data.Char
import Control.Applicative
import System.Environment

import Text.Earley

grammar :: Grammar r String (Prod r String Char [String])
grammar = mdo
  whitespace  <- rule $ () <$ many (satisfy isSpace)
  whitespace1 <- rule $ () <$ satisfy isSpace <* whitespace <?> "whitespace"

  ident <- rule 
    $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
   <?> "identifier"

  expr <- rule
    $  (:)   <$> ident <* whitespace1 <*> expr
   <|> (:[]) <$> ident <* whitespace

  return expr

main :: IO ()
main = do
  x:_ <- getArgs
  print $ fullParses (parser grammar) x
