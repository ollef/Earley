-- | Context-free grammars.
{-# LANGUAGE CPP, GADTs, RankNTypes #-}
module Text.Earley.Grammar
  ( Prod(..)
  , terminal
  , (<?>)
  , constraint
  , disambiguate
  , alts
  , Grammar(..)
  , rule
  , runGrammar
  ) where
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.String (IsString(..))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
import Data.Semigroup

infixr 0 <?>

-- | A production.
--
-- The type parameters are:
--
-- @a@: The return type of the production.
--
-- @t@ for terminal: The type of the terminals that the production operates
-- on.
--
-- @e@ for expected: The type of names, used for example to report expected
-- tokens.
--
-- @r@ for rule: The type of a non-terminal. This plays a role similar to the
-- @s@ in the type @ST s a@.  Since the 'parser' function expects the @r@ to be
-- universally quantified, there is not much to do with this parameter other
-- than leaving it universally quantified.
--
-- As an example, @'Prod' r 'String' 'Char' 'Int'@ is the type of a production that
-- returns an 'Int', operates on (lists of) characters and reports 'String'
-- names.
--
-- Most of the functionality of 'Prod's is obtained through its instances, e.g.
-- 'Functor', 'Applicative', and 'Alternative'.
data Prod r e t a where
  -- Applicative.
  Terminal    :: !(t -> Maybe a) -> !(Prod r e t (a -> b)) -> Prod r e t b
  NonTerminal :: !(r e t a) -> !(Prod r e t (a -> b)) -> Prod r e t b
  Pure        :: a -> Prod r e t a
  -- Monoid/Alternative. We have to special-case 'many' (though it can be done
  -- with rules) to be able to satisfy the Alternative interface.
  Alts        :: ![Prod r e t a] -> !(Prod r e t (a -> b)) -> Prod r e t b
  Many        :: !(Prod r e t a) -> !(Prod r e t ([a] -> b)) -> Prod r e t b
  -- Error reporting.
  Named       :: !(Prod r e t a) -> e -> Prod r e t a
  -- Non-context-free extension: conditioning on the parsed output.
  Constraint  :: !(Prod r e t a) -> (a -> Bool) -> Prod r e t a
  --
  Disamb      :: !(Prod r e t a) -> !(Prod r e t ([a] -> [b])) -> Prod r e t b

-- | Match a token for which the given predicate returns @Just a@,
-- and return the @a@.
terminal :: (t -> Maybe a) -> Prod r e t a
terminal p = Terminal p $ Pure id

-- | A named production (used for reporting expected things).
(<?>) :: Prod r e t a -> e -> Prod r e t a
(<?>) = Named

-- | A parser that filters results, post-parsing
constraint :: (a -> Bool) -> Prod r e t a -> Prod r e t a
constraint = flip Constraint

-- | Lifted instance: @(<>) = 'liftA2' ('<>')@
instance Semigroup a => Semigroup (Prod r e t a) where
  (<>) = liftA2 (Data.Semigroup.<>)

-- | Lifted instance: @mempty = 'pure' 'mempty'@
instance Monoid a => Monoid (Prod r e t a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

instance Functor (Prod r e t) where
  {-# INLINE fmap #-}
  fmap f (Terminal b p)    = Terminal b $ fmap (f .) p
  fmap f (NonTerminal r p) = NonTerminal r $ fmap (f .) p
  fmap f (Pure x)          = Pure $ f x
  fmap f (Alts as p)       = Alts as $ fmap (f .) p
  fmap f (Many p q)        = Many p $ fmap (f .) q
  fmap f (Named p n)       = Named (fmap f p) n
  fmap f (Disamb p d)      = Disamb p (fmap (fmap (fmap f)) d)

-- | Smart constructor for alternatives.
alts :: [Prod r e t a] -> Prod r e t (a -> b) -> Prod r e t b
alts as p = case as >>= go of
  []  -> empty
  [a] -> a <**> p
  as' -> Alts as' p
  where
    go (Alts [] _)         = []
    go (Alts as' (Pure f)) = fmap f <$> as'
    go (Named p' n)        = map (<?> n) $ go p'
    go a                   = [a]

instance Applicative (Prod r e t) where
  pure = Pure
  {-# INLINE (<*>) #-}
  Terminal b p    <*> q = Terminal b $ flip <$> p <*> q
  NonTerminal r p <*> q = NonTerminal r $ flip <$> p <*> q
  Pure f          <*> q = fmap f q
  Alts as p       <*> q = alts as $ flip <$> p <*> q
  Many a p        <*> q = Many a $ flip <$> p <*> q
  Named p n       <*> q = Named (p <*> q) n
  Disamb p d      <*> q = Disamb p ((\a b c -> fmap ($ b) (a c)) <$> d <*> q)

instance Alternative (Prod r e t) where
  empty = Alts [] $ pure id
  Named p m <|> q         = Named (p <|> q) m
  p         <|> Named q n = Named (p <|> q) n
  p         <|> q         = alts [p, q] $ pure id
  many (Alts [] _) = pure []
  many p           = Many p $ Pure id
  some p           = (:) <$> p <*> many p

-- | String literals can be interpreted as 'Terminal's
-- that match that string.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text (Text)
-- >>> let determiner = "the" <|> "a" <|> "an" :: Prod r e Text Text
--
instance (IsString t, Eq t, a ~ t) => IsString (Prod r e t a) where
  fromString s = Terminal f $ Pure id
    where
      fs = fromString s
      f t | t == fs = Just fs
      f _ = Nothing
  {-# INLINE fromString #-}

-- | A context-free grammar.
--
-- The type parameters are:
--
-- @a@: The return type of the grammar (often a 'Prod').
--
-- @r@ for rule: The type of a non-terminal. This plays a role similar to the
-- @s@ in the type @ST s a@.  Since the 'parser' function expects the @r@ to be
-- universally quantified, there is not much to do with this parameter other
-- than leaving it universally quantified.
--
-- Most of the functionality of 'Grammar's is obtained through its instances,
-- e.g.  'Monad' and 'MonadFix'. Note that GHC has syntactic sugar for
-- 'MonadFix': use @{-\# LANGUAGE RecursiveDo \#-}@ and @mdo@ instead of
-- @do@.
data Grammar r a where
  RuleBind :: Prod r e t a -> (Prod r e t a -> Grammar r b) -> Grammar r b
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

-- | Create a new non-terminal by giving its production.
rule :: Prod r e t a -> Grammar r (Prod r e t a)
rule p = RuleBind p return

-- | Create a non-terminal which is able to disambiguate possible parses
disambiguate :: ([a] -> b) -> Prod r e t a -> Grammar r (Prod r e t b)
disambiguate d p = do
  r <- rule p
  pure $ Disamb r (Pure (pure . d))

-- | Run a grammar, given an action to perform on productions to be turned into
-- non-terminals.
runGrammar :: MonadFix m
           => (forall e t a. Prod r e t a -> m (Prod r e t a))
           -> Grammar r b -> m b
runGrammar r grammar = case grammar of
  RuleBind p k -> do
    nt <- r p
    runGrammar r $ k nt
  Return a -> return a
  FixBind f k -> do
    a <- mfix $ runGrammar r <$> f
    runGrammar r $ k a
