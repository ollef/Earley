-- | Derived operators.
module Text.Earley.Derived where
import Control.Applicative hiding (many)
import Data.ListLike (ListLike)
import qualified Data.ListLike as ListLike

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

{-# INLINE word' #-}
word' :: (ListLike i t, Eq t) => i -> Prod r e t [t]
word' = foldr (\a b -> liftA2 (:) (satisfy (a ==)) b) (pure []) . ListLike.toList
