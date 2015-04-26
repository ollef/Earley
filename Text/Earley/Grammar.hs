-- | Context-free grammars.
{-# LANGUAGE GADTs, RankNTypes #-}
module Text.Earley.Grammar
  ( Prod(..)
  , satisfy
  , (<?>)
  , Grammar(..)
  , rule
  ) where
import Control.Applicative
import Control.Monad
import Control.Monad.Fix

infixr 0 <?>

-- | A production.
--
-- The type parameters are:
--
-- @a@: The return type of the production.
--
-- @t@: The type of the terminals that the production operates on.
--
-- @e@: The type of names, used for example to report expected tokens.
--
-- @r@: The type of a non-terminal. This plays a role similar to the @s@ in the
--      type @ST s a@.  Since the 'parser' function expects the @r@ to be
--      universally quantified, there is not much to do with this parameter
--      other than leaving it universally quantified.
--
-- As an example, @'Prod' r 'String' 'Char' 'Int'@ is the type of a production that
-- returns an 'Int', operates on (lists of) characters and reports 'String'
-- names.
--
-- Most of the functionality of 'Prod's is obtained through its instances, e.g.
-- 'Functor', 'Applicative', and 'Alternative'.
data Prod r e t a where
  -- Applicative.
  Terminal    :: !(t -> Bool) -> !(Prod r e t (t -> b)) -> Prod r e t b
  NonTerminal :: !(r e t a) -> !(Prod r e t (a -> b)) -> Prod r e t b
  Pure        :: a -> Prod r e t a
  -- Monoid/Alternative. We have to special-case 'many' (though it can be done
  -- with rules) to be able to satisfy the Alternative interface.
  Plus        :: !(Prod r e t a) -> !(Prod r e t a) -> Prod r e t a
  Many        :: !(Prod r e t a) -> !(Prod r e t ([a] -> b)) -> Prod r e t b
  Empty       :: Prod r e t a
  -- Error reporting.
  Named       :: !(Prod r e t a) -> e -> Prod r e t a

-- | Match a token that satisfies the given predicate. Returns the matched token.
{-# INLINE satisfy #-}
satisfy :: (t -> Bool) -> Prod r e t t
satisfy p = Terminal p $ Pure id

-- | A named production (used for reporting expected things).
(<?>) :: Prod r e t a -> e -> Prod r e t a
(<?>) = Named

instance Monoid (Prod r e t a) where
  mempty  = empty
  mappend = (<|>)

instance Functor (Prod r e t) where
  {-# INLINE fmap #-}
  fmap f (Terminal b p)    = Terminal b $ fmap (f .) p
  fmap f (NonTerminal r p) = NonTerminal r $ fmap (f .) p
  fmap f (Pure x)          = Pure $ f x
  fmap f (Plus p q)        = Plus (fmap f p) (fmap f q)
  fmap f (Many p q)        = Many p $ fmap (f .) q
  fmap _ Empty             = Empty
  fmap f (Named p n)       = Named (fmap f p) n

instance Applicative (Prod r e t) where
  pure = Pure
  {-# INLINE (<*>) #-}
  Terminal b p    <*> q = Terminal b $ flip <$> p <*> q
  NonTerminal r p <*> q = NonTerminal r $ flip <$> p <*> q
  Pure f          <*> q = fmap f q
  Plus a b        <*> q = a <*> q <|> b <*> q
  Many a p        <*> q = Many a $ flip <$> p <*> q
  Empty           <*> _ = Empty
  Named p n       <*> q = Named (p <*> q) n

instance Alternative (Prod r e t) where
  empty = Empty
  Empty     <|> q         = q
  p         <|> Empty     = p
  Named p m <|> q         = Named (p <|> q) m
  p         <|> Named q n = Named (p <|> q) n
  p         <|> q         = Plus p q
  many Empty = pure []
  many p     = Many p $ Pure id
  some p     = (:) <$> p <*> many p

-- | A context-free grammar.
--
-- The type parameters are:
--
-- @a@: The return type of the grammar (often a 'Prod').
--
-- @e@: The type of names, used for example to report expected tokens.
--
-- @r@: The type of a non-terminal. This plays a role similar to the @s@ in the
--      type @ST s a@.  Since the 'parser' function expects the @r@ to be
--      universally quantified, there is not much to do with this parameter
--      other than leaving it universally quantified.
--
-- Most of the functionality of 'Grammar's is obtained through its instances,
-- e.g.  'Monad' and 'MonadFix'. Note that GHC has syntactic sugar for
-- 'MonadFix': use @{-\# LANGUAGE RecursiveDo \#-}@ and @mdo@ instead of
-- @do@.
data Grammar r e a where
  RuleBind :: Prod r e t a -> (Prod r e t a -> Grammar r e b) -> Grammar r e b
  FixBind  :: (a -> Grammar r e a) -> (a -> Grammar r e b) -> Grammar r e b
  Return   :: a -> Grammar r e a

instance Functor (Grammar r e) where
  fmap f (RuleBind ps h) = RuleBind ps (fmap f . h)
  fmap f (FixBind g h)   = FixBind g (fmap f . h)
  fmap f (Return x)      = Return $ f x

instance Applicative (Grammar r e) where
  pure  = return
  (<*>) = ap

instance Monad (Grammar r e) where
  return = Return
  RuleBind ps f >>= k = RuleBind ps (f >=> k)
  FixBind f g   >>= k = FixBind f (g >=> k)
  Return x      >>= k = k x

instance MonadFix (Grammar r e) where
  mfix f = FixBind f return

-- | Create a new non-terminal by giving its production.
rule :: Prod r e t a -> Grammar r e (Prod r e t a)
rule p = RuleBind p return
