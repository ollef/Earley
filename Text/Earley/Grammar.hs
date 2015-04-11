-- | Context-free grammars
{-# LANGUAGE GADTs, RankNTypes #-}
module Text.Earley.Grammar
  ( Prod(..)
  , satisfy
  , Grammar(..)
  , rule
  ) where
import Control.Applicative
import Control.Monad
import Control.Monad.Fix

-- | A production with terminals of type @t@, return type @a@, and
--   non-terminals of type @r t x@ for some @x@.
data Prod r t a where
  Terminal    :: !(t -> Bool) -> !(Prod r t (t -> b)) -> Prod r t b
  NonTerminal :: !(r t a) -> !(Prod r t (a -> b)) -> Prod r t b
  Pure        :: a -> Prod r t a

-- | Match a token that satisfies the given predicate. Returns the matched token.
{-# INLINE satisfy #-}
satisfy :: (t -> Bool) -> Prod r t t
satisfy p = Terminal p $ Pure id

instance Functor (Prod r t) where
  {-# INLINE fmap #-}
  fmap f (Terminal b p)    = Terminal b $ fmap (f .) p
  fmap f (NonTerminal r p) = NonTerminal r $ fmap (f .) p
  fmap f (Pure x)          = Pure $ f x

instance Applicative (Prod r t) where
  pure = Pure
  {-# INLINE (<*>) #-}
  Terminal b p    <*> q = Terminal b    (flip <$> p <*> q)
  NonTerminal r p <*> q = NonTerminal r (flip <$> p <*> q)
  Pure f          <*> q = fmap f q

-- | The 'Grammar' monad: A representation of a context-free grammars.
data Grammar r a where
  RuleBind :: [Prod r t a] -> (Prod r t a -> Grammar r b) -> Grammar r b
  FixBind  :: (a -> Grammar r a) -> (a -> Grammar r b) -> Grammar r b
  Return   :: a -> Grammar r a

instance Functor (Grammar r) where
  fmap f (RuleBind ps h) = RuleBind ps (fmap f . h)
  fmap f (FixBind g h)   = FixBind g (fmap f . h)
  fmap f (Return x)      = Return $ f x

instance Applicative (Grammar r) where
  pure  = return
  (<*>) = ap

instance Monad (Grammar r) where
  return = Return
  RuleBind ps f >>= k = RuleBind ps (f >=> k)
  FixBind f g   >>= k = FixBind f (g >=> k)
  Return x      >>= k = k x

instance MonadFix (Grammar r) where
  mfix f = FixBind f return

-- | Create a new non-terminal by listing its production rules. Note that
-- 'Grammar' is an instance of 'MonadFix' which is to be used to construct
-- recursive rules.
rule :: [Prod r t a] -> Grammar r (Prod r t a)
rule ps = RuleBind ps return
