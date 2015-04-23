-- | Derived operators.
module Text.Earley.Derived where
import Control.Applicative hiding (many)

import Text.Earley.Grammar

-- | Match a single token.
symbol :: Eq t => t -> Prod r e t t
symbol x = satisfy (== x)

-- | Match a single token and give it the name of the token.
namedSymbol :: Eq t => t -> Prod r t t t
namedSymbol x = symbol x <?> x

-- | Match a list of tokens in sequence.
{-# INLINE word #-}
word :: Eq t => [t] -> Prod r e t [t]
word = foldr (liftA2 (:) . satisfy . (==)) (pure [])
