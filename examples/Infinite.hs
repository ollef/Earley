{-# LANGUAGE RecursiveDo #-}
module Testa where
import Control.Applicative
import Text.Earley

grammar :: Grammar r (Prod r () Char [Maybe Char])
grammar = mdo
  as <- rule $ pure []
            <|> (:) <$> optional (symbol 'a') <*> as
  return as

-- This grammar has an infinite number of results. We can still recognise the
-- language, i.e. get a report, but we can't get the results, because in doing
-- so the library will try to force a circular value.
main :: IO ()
main = do
  let input = "aaa"
  print $ report (parser grammar) input -- Works
  print $ fullParses (parser grammar) input -- Hangs
