{-# LANGUAGE Rank2Types #-}
-- | Derived operators.
module Text.Earley.Derived where
import Control.Applicative hiding (many)
import Control.Monad (guard)
import Data.ListLike(ListLike)
import qualified Data.ListLike as ListLike

import Text.Earley.Grammar
import Text.Earley.Parser
import Data.Functor.Identity (Identity)

-- | Match a token that satisfies the given predicate. Returns the matched
-- token. See also 'terminal'.
{-# INLINE satisfy #-}
satisfy :: (t -> Bool) -> Prod r m e t t
satisfy p = terminal ((<$) <*> guard . p)

-- | Match a single token.
token :: Eq t => t -> Prod r m e t t
token x = satisfy (== x)

-- | Match a single token and give it the name of the token.
namedToken :: Eq t => t -> Prod r m t t t
namedToken x = token x <?> x

-- | Match a single token with any value
anyToken :: Prod r m e t t
anyToken = terminal Just

-- | Match a list of tokens in sequence.
{-# INLINE list #-}
list :: Eq t => [t] -> Prod r m e t [t]
list = listLike

-- | Match a 'ListLike' of tokens in sequence.
{-# INLINE listLike #-}
listLike :: (Eq t, ListLike i t) => i -> Prod r m e t i
listLike = ListLike.foldr (liftA2 ListLike.cons . satisfy . (==)) (pure ListLike.empty)

-- | Whether or not the grammar matches the input string. Equivalently,
-- whether the given input is in the language described by the grammars.
matches :: ListLike i t => (forall r. Grammar r Identity (Prod r Identity e t a)) -> i -> Bool
matches grammar = not . null . fst . fullParses (parser grammar)
