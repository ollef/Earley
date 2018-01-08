{-# LANGUAGE RecursiveDo #-}
module InfiniteEpsilon where

import Control.Applicative
import Text.Earley

grammar :: Grammar r (Prod r e t Int)
grammar = mdo
  r <- rule $ pure 1 <|> (+1) <$> r
  pure r

grammar2 :: Grammar r (Prod r e t Int)
grammar2 = mdo
  r <- rule $ pure 1 <|> r
  return r

grammar3 :: Grammar r (Prod r e t Int)
grammar3 = mdo
  r <- rule $ (+1) <$> r <|> pure 1
  pure r

grammar4 :: Grammar r (Prod r e t Int)
grammar4 = mdo
  r <- rule $ r <|> pure 1
  return r
