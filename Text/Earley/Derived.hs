{-# LANGUAGE RecursiveDo #-}
-- | Derived operators
module Text.Earley.Derived where
import Control.Applicative hiding (many)

import Text.Earley.Grammar

-- | Match a single token.
symbol :: Eq t => t -> Prod r t t
symbol x = satisfy (== x)

-- | Match a list of tokens in sequence.
{-# INLINE word #-}
word :: Eq t => [t] -> Prod r t [t]
word = foldr (liftA2 (:) . symbol) (pure [])

-- | Create a rule that matches zero or more occurences of the given
--   production.
{-# INLINE many #-}
many :: Prod r t a -> Grammar r (Prod r t [a])
many p = mdo
  ps <- rule [pure [], (:) <$> p <*> ps]
  return ps

-- | Create a rule that matches one or more occurences of the given production.
{-# INLINE some #-}
some :: Prod r t a -> Grammar r (Prod r t [a])
some p = mdo
  ps0 <- many p
  rule [(:) <$> p <*> ps0]

-- ** Applicative style combinators for predicate matching
infixl 4 <$>., <*>., <$., <*.

{-# INLINE (<$>.) #-}
{-# INLINE (<*>.) #-}
{-# INLINE (<$.) #-}
{-# INLINE (<*.) #-}
(<$>.) :: (a -> b) -> (a -> Bool) -> Prod r a b
f <$>. g = f <$> satisfy g

(<*>.) :: Prod r a (a -> b) -> (a -> Bool) -> Prod r a b
f <*>. g = f <*> satisfy g

(<$.) :: a -> (b -> Bool) -> Prod r b a
f <$. g = f <$ satisfy g

(<*.) :: Prod r b a -> (b -> Bool) -> Prod r b a
f <*. g = f <* satisfy g
