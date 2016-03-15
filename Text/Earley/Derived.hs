-- | Derived operators.
module Text.Earley.Derived where
import Control.Applicative hiding (many)
import Data.ListLike(ListLike)
import qualified Data.ListLike as ListLike

import Text.Earley.Grammar

-- | Match a token that satisfies the given predicate. Returns the matched
-- token.
{-# INLINE satisfy #-}
satisfy :: (t -> Bool) -> Prod r e t t
satisfy p = Terminal f $ Pure id
  where
    f t | p t = Just t
    f _       = Nothing

-- | Match a single token.
token :: Eq t => t -> Prod r e t t
token x = satisfy (== x)

-- | Match a single token and give it the name of the token.
namedToken :: Eq t => t -> Prod r t t t
namedToken x = token x <?> x

-- | Match a list of tokens in sequence.
{-# INLINE list #-}
list :: Eq t => [t] -> Prod r e t [t]
list = foldr (liftA2 (:) . satisfy . (==)) (pure [])

-- | Match a 'ListLike' of tokens in sequence.
{-# INLINE listLike #-}
listLike :: (Eq t, ListLike i t) => i -> Prod r e t i
listLike = ListLike.foldr (liftA2 ListLike.cons . satisfy . (==)) (pure ListLike.empty)

{-# DEPRECATED symbol "Use `token` instead" #-}
symbol :: Eq t => t -> Prod r e t t
symbol = token

{-# DEPRECATED namedSymbol "Use `namedToken` instead" #-}
namedSymbol :: Eq t => t -> Prod r e t t
namedSymbol = token

{-# DEPRECATED word "Use `list` or `listLike` instead" #-}
word :: Eq t => [t] -> Prod r e t [t]
word = list
