{-# LANGUAGE RecursiveDo #-}
module Text.Earley.Mixfix where

import Data.Either
import Data.Foldable(asum, foldrM)
import Text.Earley

data Associativity
  = LeftAssoc
  | NonAssoc
  | RightAssoc
  deriving (Eq, Show)

-- | An identifier with identifier parts ('Just's), and holes ('Nothing's)
-- representing the positions of its arguments.
--
-- Example (commonly written "if_then_else_"):
-- @['Just' "if", Nothing, 'Just' "then", Nothing, 'Just' "else", Nothing] :: 'Holey' 'String'@
type Holey a = [Maybe a]

-- | Create a grammar for parsing mixfix expressions.
mixfixExpression
  :: [[(Holey (Prod r e t ident), Associativity)]]
  -- ^ A table of holey identifier parsers, with associativity information.
  -- The identifiers should be in groups of precedence levels listed from
  -- binding the least to the most tightly.
  --
  -- The associativity is taken into account when an identifier starts or
  -- ends with a hole, or both. Internal holes (e.g. after "if" in
  -- "if_then_else_") start from the beginning of the table.
  -> Prod r e t expr
  -- ^ An atom, i.e. what is parsed at the lowest level. This will
  -- commonly be a (non-mixfix) identifier or a parenthesised expression.
  -> (Holey ident -> [expr] -> expr)
  -- ^ How to combine the successful application of a holey identifier to its
  -- arguments into an expression.
  -> Grammar r e (Prod r e t expr)
mixfixExpression table atom app = mdo
  expr <- foldrM ($) atom $ map (level expr) table
  return expr
  where
    app' xs = app (either (const Nothing) Just <$> xs) $ lefts xs
    level expr idents next = mdo
      same <- rule $ asum $ next : map (mixfixIdent same) idents
      return same
      where
        cons p q = (:) <$> p <*> q
        mixfixIdent same (ps, a) = app' <$> go ps
          where
            go ps' = case ps' of
              [] -> pure mempty
              [Just p] -> pure . Right <$> p
              Nothing:rest -> cons (Left <$> if a == RightAssoc then next
                                                                else same)
                $ go rest
              [Just p, Nothing] -> cons (Right <$> p)
                $ pure . Left <$> if a == LeftAssoc then next else same
              Just p:Nothing:rest -> cons (Right <$> p)
                $ cons (Left <$> expr)
                $ go rest
              Just p:rest@(Just _:_) -> cons (Right <$> p) $ go rest

