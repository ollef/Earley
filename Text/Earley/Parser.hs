-- | Parsing
{-# LANGUAGE BangPatterns, DeriveFunctor, GADTs, Rank2Types #-}
module Text.Earley.Parser
  ( Result(..)
  , parser
  , allParses
  , fullParses
  ) where
import Control.Applicative
import Control.Arrow
import Control.Monad.Fix
import Control.Monad.ST.Lazy
import Data.Functor.Yoneda
import Data.ListLike(ListLike)
import qualified Data.ListLike as ListLike
import Data.STRef.Lazy
import Text.Earley.Grammar

-------------------------------------------------------------------------------
-- * Concrete rules and productions
-------------------------------------------------------------------------------
-- | The concrete rule type that the parser uses
data Rule s r e t a = Rule
  { ruleProd     :: ProdR s r e t a
  , ruleNullable :: {-# UNPACK #-} !(STRef s (Maybe [a]))
  , ruleConts    :: {-# UNPACK #-} !(STRef s (Conts s r e t a r))
  }

type ProdR s r e t a = Prod (Rule s r) e t a

nullable :: Rule s r e t a -> ST s [a]
nullable r = do
  mn <- readSTRef $ ruleNullable r
  case mn of
    Just xs -> return xs
    Nothing -> do
      writeSTRef (ruleNullable r) $ Just mempty
      res <- nullableProd $ ruleProd r
      writeSTRef (ruleNullable r) $ Just res
      return res

nullableProd :: ProdR s r e t a -> ST s [a]
nullableProd (Terminal _ _)    = return mempty
nullableProd (NonTerminal r p) = do
  as <- nullable r
  concat <$> mapM (\a -> nullableProd $ fmap ($ a) p) as
nullableProd (Pure a)          = return [a]
nullableProd (Plus a b)        = mappend <$> nullableProd a <*> nullableProd b
nullableProd (Many p q)        = do
  as <- nullableProd $ (:[]) <$> p <|> pure []
  concat <$> mapM (\a -> nullableProd $ fmap ($ a) q) as
nullableProd Empty             = return mempty
nullableProd (Named p _)       = nullableProd p

-------------------------------------------------------------------------------
-- * States and continuations
-------------------------------------------------------------------------------
type Pos = Int

-- | An Earley state with result type @a@.
data State s r e t a where
  State :: {-# UNPACK #-} !Pos
        -> !(ProdR s r e t b)
        -> {-# UNPACK #-} !(Conts s r e t b a)
        -> State s r e t a
  Final :: a -> State s r e t a

-- | A continuation accepting an @a@ and producing a @b@.
data Cont s r e t a b where
  Cont      :: {-# UNPACK #-} !Pos
            -> !(ProdR s r e t (a -> b))
            -> {-# UNPACK #-} !(Conts s r e t b c)
            -> Cont s r e t a c
  FinalCont :: (a -> c) -> Cont s r e t a c

type Conts s r e t a c = STRef s [Cont s r e t a c]

contraMapCont :: (b -> a) -> Cont s r e t a c -> Cont s r e t b c
contraMapCont f (Cont pos p cs) = (Cont pos $! ((. f) <$> p)) cs
contraMapCont f (FinalCont g)   = FinalCont (g . f)

contToState :: a -> Cont s r e t a c -> State s r e t c
contToState a (Cont pos p cs) = State pos (($ a) <$> p) cs
contToState a (FinalCont f)   = Final (f a)

-- | Strings of non-ambiguous continuations can be optimised by removing
--   indirections.
simplifyCont :: Conts s r e t b a -> ST s [Cont s r e t b a]
simplifyCont cont = readSTRef cont >>= go False
  where
    go !_ [Cont _ (Pure f) cont'] = do
      ks' <- simplifyCont cont'
      go True $ map (contraMapCont f) ks'
    go True ks = do
      writeSTRef cont ks
      return ks
    go False ks = return ks

-------------------------------------------------------------------------------
-- * Grammars
-------------------------------------------------------------------------------
-- | Interpret an abstract 'Grammar'.
grammar :: Grammar (Rule s r) e a -> ST s a
grammar g = case g of
  RuleBind p k -> do
    c  <- newSTRef =<< newSTRef mempty
    nr <- newSTRef Nothing
    grammar $ k $ NonTerminal (Rule p nr c) $ Pure id
  FixBind f k   -> do
    a <- mfix $ fmap grammar f
    grammar $ k a
  Return x      -> return x

-- | Given a grammar, construct an initial state.
initialState :: ProdR s a e t a -> ST s (State s a e t a)
initialState r = do
  rs <- newSTRef [FinalCont id]
  return $ State (-1) r rs

-------------------------------------------------------------------------------
-- * Parsing
-------------------------------------------------------------------------------
-- | A parsing report, which contains fields that are useful for presenting
-- errors to the user if a parse is deemed a failure.  Note however that we get
-- a report even when we successfully parse something.
data Report e i = Report
  { position   :: Int -- ^ The final position in the input (0-based) that the
                      -- parser reached.
  , expected   :: [e] -- ^ The named productions processed at the last
                      -- position.
  , unconsumed :: i   -- ^ The part of the input string that was not consumed,
                      -- which may be empty.
  } deriving Show

-- | The result of a parse.
data Result s e i a
  = Ended (Report e i)
    -- ^ The parser ended.
  | Parsed a Int i (i -> ST s (Result s e i a))
    -- ^ The parser parsed something, namely an 'a'. The 'Int' is the position
    -- in the input where it did so, the 'i' is the rest of the input, and the
    -- function is the parser continuation. This allows incrementally feeding
    -- the parser more input (e.g. when the 'i' is empty).
  deriving (Functor)

{-# INLINE uncons #-}
uncons :: ListLike i t => i -> Maybe (t, i)
uncons i
  | ListLike.null i = Nothing
  | otherwise       = Just (ListLike.head i, ListLike.tail i)

{-# INLINE safeTail #-}
safeTail :: ListLike i t => i -> i
safeTail ts'
  | ListLike.null ts' = ts'
  | otherwise         = ListLike.tail ts'

{-# SPECIALISE parse :: [State s a e t a] -> [State s a e t a] -> ST s () -> [e] -> Pos -> [t] -> ST s (Result s e [t] a) #-}
-- | The internal parsing routine
parse :: ListLike i t
      => [State s a e t a] -- ^ States to process at this position
      -> [State s a e t a] -- ^ States to process at the next position
      -> ST s ()           -- ^ Computation that resets the continuation refs of productions
      -> [e]               -- ^ Named productions encountered at this position
      -> Pos               -- ^ The current position in the input string
      -> i                 -- ^ The input string
      -> ST s (Result s e i a)
parse []      []    !reset names !pos   !ts = do
  reset
  return $ Ended Report {position = pos, expected = names, unconsumed = ts}
parse []      !next !reset _names !pos !ts = do
  reset
  parse next [] (return ()) [] (pos + 1) (safeTail ts)
parse (st:ss) !next !reset names !pos !ts = case st of
  Final a -> return $ Parsed a pos ts $ parse ss next reset names pos
  State spos pr scont -> case pr of
    Terminal f p -> case uncons ts of
      Just (t, _) | f t -> parse ss (State spos (($ t) <$> p) scont : next) reset names pos ts
      _                 -> parse ss next reset names pos ts
    NonTerminal r p -> do
      rkref <- readSTRef $ ruleConts r
      ks    <- readSTRef rkref
      writeSTRef rkref (Cont spos p scont : ks)
      nulls' <- nullable r
      let notExpanded = null ks
          p'          = liftYoneda p
          nulls       = fmap (\a -> State spos (lowerYoneda $ ($ a) <$> p') scont) nulls'
      if notExpanded then do
        let st' = State pos (ruleProd r) rkref
        parse (st' : nulls ++ ss)
              next
              ((writeSTRef (ruleConts r) =<< newSTRef mempty) >> reset)
              names
              pos
              ts
      else
        parse (nulls ++ ss) next reset names pos ts
    Pure a | spos /= pos -> do
      conts <- simplifyCont scont
      parse (map (contToState a) conts ++ ss) next reset names pos ts
           | otherwise -> parse ss next reset names pos ts

    Plus p q    -> parse (State spos p scont : State spos q scont : ss) next reset names pos ts
    Many p q    -> do
      rkref <- newSTRef [Cont spos (Many p ((\f as a -> f (a : as)) <$> q)) scont]
      let st' = State pos p rkref
          nst = State spos (($ []) <$> q) scont
      parse (st' : nst : ss) next reset names pos ts
    Empty       -> parse ss next reset names pos ts

    Named pr' n -> parse (State spos pr' scont : ss) next reset (n : names) pos ts

{-# INLINE parser #-}
-- | Create a parser from the given grammar.
parser :: ListLike i t
       => (forall r. Grammar r e (Prod r e t a))
       -> i
       -> ST s (Result s e i a)
parser g xs = do
  s <- initialState =<< grammar g
  parse [s] [] (return ()) [] 0 xs

-- | Return all parses from the result of a given parser. The result may
-- contain partial parses. The 'Int's are the position at which a result was
-- produced.
allParses :: (forall s. ST s (Result s e i a)) -> ([(a, Int)], Report e i)
allParses p = runST $ p >>= go
  where
    go :: Result s e i a -> ST s ([(a, Int)], Report e i)
    go r = case r of
      Ended report     -> return ([], report)
      Parsed a pos i k -> fmap (first ((a, pos) :)) $ go =<< k i

{-# INLINE fullParses #-}
-- | Return all parses that reached the end of the input from the result of a
--   given parser.
fullParses :: ListLike i t => (forall s. ST s (Result s e i a)) -> ([a], Report e i)
fullParses p = runST $ p >>= go
  where
    go :: ListLike i t => Result s e i a -> ST s ([a], Report e i)
    go r = case r of
      Ended report -> return ([], report)
      Parsed a _ i k
        | ListLike.null i -> fmap (first (a :)) $ go =<< k i
        | otherwise       -> go =<< k i
