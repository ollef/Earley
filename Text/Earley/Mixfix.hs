{-# LANGUAGE CPP, RecursiveDo #-}
module Text.Earley.Mixfix
  ( Associativity(..)
  , Holey
  , mixfixExpression
  , mixfixExpressionSeparate
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Traversable(sequenceA)
#endif
import Data.Either
import Data.Foldable(asum, foldrM)
import Text.Earley

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n = sequenceA . replicate n

consA :: Applicative f => f a -> f [a] -> f [a]
consA p q = (:) <$> p <*> q

data Associativity
  = LeftAssoc
  | NonAssoc
  | RightAssoc
  deriving (Eq, Show)

-- | An identifier with identifier parts ('Just's), and holes ('Nothing's)
-- representing the positions of its arguments.
--
-- Example (commonly written "if_then_else_"):
-- @['Just' "if", 'Nothing', 'Just' "then", 'Nothing', 'Just' "else", 'Nothing'] :: 'Holey' 'String'@
type Holey a = [Maybe a]

-- | Create a grammar for parsing mixfix expressions.
mixfixExpression
  :: [[(Holey (Prod r e t ident), Associativity)]]
  -- ^ A table of holey identifier parsers, with associativity information.
  -- The identifiers should be in groups of precedence levels listed from
  -- binding the least to the most tightly.
  --
  -- The associativity is taken into account when an identifier starts or ends
  -- with holes, or both. Internal holes (e.g. after "if" in "if_then_else_")
  -- start from the beginning of the table.
  --
  -- Note that this rule also applies to identifiers with multiple consecutive
  -- holes, e.g. "if__" --- the associativity then applies to both holes.
  -> Prod r e t expr
  -- ^ An atom, i.e. what is parsed at the lowest level. This will
  -- commonly be a (non-mixfix) identifier or a parenthesised expression.
  -> (Holey ident -> [expr] -> expr)
  -- ^ How to combine the successful application of a holey identifier to its
  -- arguments into an expression.
  -> Grammar r (Prod r e t expr)
mixfixExpression table atom app = mixfixExpressionSeparate table' atom
  where
    table' = [[(holey, assoc, app) | (holey, assoc) <- row] | row <- table]

-- | A version of 'mixfixExpression' with a separate semantic action for each
-- individual 'Holey' identifier.
mixfixExpressionSeparate
  :: [[(Holey (Prod r e t ident), Associativity, Holey ident -> [expr] -> expr)]]
  -- ^ A table of holey identifier parsers, with associativity information and
  -- semantic actions.  The identifiers should be in groups of precedence
  -- levels listed from binding the least to the most tightly.
  --
  -- The associativity is taken into account when an identifier starts or ends
  -- with holes, or both. Internal holes (e.g. after "if" in "if_then_else_")
  -- start from the beginning of the table.
  --
  -- Note that this rule also applies to identifiers with multiple consecutive
  -- holes, e.g. "if__" --- the associativity then applies to both holes.
  -> Prod r e t expr
  -- ^ An atom, i.e. what is parsed at the lowest level. This will
  -- commonly be a (non-mixfix) identifier or a parenthesised expression.
  -> Grammar r (Prod r e t expr)
mixfixExpressionSeparate table atom = mdo
  expr <- foldrM ($) atom $ map (level expr) table
  return expr
  where
    level expr idents next = mdo
      same <- rule $ asum $ next : map (mixfixIdent same) idents
      return same
      where
        -- Group consecutive holes and ident parts.
        grp [] = []
        grp (Nothing:ps) = case grp ps of
          Left n:rest -> (Left $! (n + 1)) : rest
          rest        -> Left 1            : rest
        grp (Just p:ps) = case grp ps of
          Right ps':rest -> Right (consA p ps')       : rest
          rest           -> Right (consA p $ pure []) : rest

        mixfixIdent same (ps, a, f) = f' <$> go (grp ps)
          where
            f' xs = f (concatMap (either (map $ const Nothing) $ map Just) xs)
                   $ concat $ lefts xs
            go ps' = case ps' of
              [] -> pure []
              [Right p] -> pure . Right <$> p
              Left n:rest -> consA
                (Left <$> replicateA n (if a == RightAssoc then next
                                                           else same))
                $ go rest
              [Right p, Left n] -> consA
                (Right <$> p)
                $ pure . Left <$> replicateA n (if a == LeftAssoc then next
                                                                  else same)
              Right p:Left n:rest -> consA (Right <$> p)
                $ consA (Left <$> replicateA n expr)
                $ go rest
              Right _:Right _:_ -> error
                $  "Earley.mixfixExpression: The impossible happened. "
                ++ "Please report this as a bug."
