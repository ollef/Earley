{-# LANGUAGE GeneralizedNewtypeDeriving, RecursiveDo #-}
module Text.Earley.GrammarUnsafe
  ( NonTerminalRef
  , Grammar
  , grammar
  , rule
  ) where

import Control.Applicative
import Data.IORef
import System.IO.Unsafe

import Text.Earley.Grammar (Prod(..))
import qualified Text.Earley.Grammar as G

newtype NonTerminalRef r t a = NonTerminalRef (IORef (Either (Prod r t a) [Grammar r t a]))

newtype Grammar r t a = Grammar (Prod (NonTerminalRef r) t a)
  deriving (Functor, Applicative)

{-# NOINLINE rule #-}
rule :: [Grammar r t a] -> Grammar r t a
rule ps = unsafePerformIO $ do
  r <- newIORef $ Right ps
  return $ Grammar $ NonTerminal (NonTerminalRef r) $ Pure id

instance Alternative (Grammar r t) where
  empty   = rule []
  p <|> q = rule [p, q]
  some a  = (:) <$> a <*> many a
  many a  = let ma = pure [] <|> (:) <$> a <*> ma in ma

grammar :: Grammar r t a -> G.Grammar r (Prod r t a)
grammar (Grammar prod) = case prod of
  Terminal b p -> do
    p' <- grammar $ Grammar p
    return $ Terminal b p'
  NonTerminal (NonTerminalRef ref) q -> do
    q' <- grammar $ Grammar q
    case unsafePerformIO $ readIORef ref of
      Left p   -> return $ p <**> q'
      Right ps -> mdo
        () <- return $ unsafePerformIO $ writeIORef ref $ Left r
        r <- G.rule ps'
        ps' <- mapM grammar ps
        return $ r <**> q'
  Pure a -> return $ Pure a
