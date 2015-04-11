-- | Parsing
{-# LANGUAGE BangPatterns, DeriveFunctor, GADTs, Rank2Types #-}
module Text.Earley.Parser
  ( Result(..)
  , parser
  , allParses
  , fullParses
  ) where
import Control.Applicative
import Control.Monad.Fix
import Control.Monad.ST.Lazy
import Data.Functor.Yoneda
import Data.ListLike(ListLike)
import qualified Data.ListLike as ListLike
import Data.Monoid
import Data.STRef.Lazy
import Text.Earley.Grammar

-------------------------------------------------------------------------------
-- * Concrete rules and productions
-------------------------------------------------------------------------------
-- | The concrete rule type that the parser uses
data Rule s r t a = Rule
  { ruleProds    :: ![ProdR s r t a]
  , ruleNullable :: !(STRef s (Maybe [a]))
  , ruleConts    :: !(STRef s (Conts s r t a r))
  }

type ProdR s r t a = Prod (Rule s r) t a

nullable :: Rule s r t a -> ST s [a]
nullable r = do
  mn <- readSTRef $ ruleNullable r
  case mn of
    Just xs -> return xs
    Nothing -> do
      writeSTRef (ruleNullable r) $ Just mempty
      res <- concat <$> mapM nullableProd (ruleProds r)
      writeSTRef (ruleNullable r) $ Just res
      return res

nullableProd :: ProdR s r t a -> ST s [a]
nullableProd (Terminal _ _)    = return mempty
nullableProd (NonTerminal r p) = do
  as <- nullable r
  concat <$> mapM (\a -> nullableProd (fmap ($ a) p)) as
nullableProd (Pure a)          = return [a]

-------------------------------------------------------------------------------
-- * States and continuations
-------------------------------------------------------------------------------
type Pos = Int

-- | An Earley state with result type @a@.
data State s r t a where
  State :: {-# UNPACK #-} !Pos
        -> !(ProdR s r t b)
        -> !(Conts s r t b a)
        -> State s r t a
  Final :: a -> State s r t a

-- | A continuation accepting an @a@ and producing a @b@.
data Cont s r t a b where
  Cont      :: {-# UNPACK #-} !Pos
            -> !(ProdR s r t (a -> b))
            -> !(Conts s r t b c)
            -> Cont s r t a c
  FinalCont :: (a -> c) -> Cont s r t a c

type Conts s r t a c = STRef s [Cont s r t a c]

contraMapCont :: (b -> a) -> Cont s r t a c -> Cont s r t b c
contraMapCont f (Cont pos p cs) = (Cont pos $! ((. f) <$> p)) cs
contraMapCont f (FinalCont g)   = FinalCont (g . f)

contToState :: a -> Cont s r t a c -> State s r t c
contToState a (Cont pos p cs) = State pos (($ a) <$> p) cs
contToState a (FinalCont f)   = Final (f a)

-- | Strings of non-ambiguous continuations can be optimised by removing
--   indirections.
simplifyCont :: Conts s r t b a -> ST s [Cont s r t b a]
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
type GrammarState s = STRef s Int

-- | A concrete grammar type (that the 'Grammar' monad can be interpreted to).
type GrammarM s a = GrammarState s -> ST s a

initialGrammarState :: ST s (GrammarState s)
initialGrammarState = newSTRef 0

-- | Interpret an abstract 'Grammar' into a concrete 'GrammarM'.
grammarM :: Grammar (Rule s r) a -> GrammarM s a
grammarM g s = case g of
  RuleBind ps k -> do
    c  <- newSTRef =<< newSTRef mempty
    nr <- newSTRef Nothing
    grammarM (k $ NonTerminal (Rule ps nr c) $ Pure id) s
  FixBind f k   -> do
    a <- mfix $ fmap (`grammarM` s) f
    grammarM (k a) s
  Return x      -> return x

-- | Given a grammar, construct an initial state.
initialState :: GrammarM s (ProdR s a t a) -> ST s (State s a t a)
initialState g = do
  r  <- g =<< initialGrammarState
  rs <- newSTRef [FinalCont id]
  return $ State (-1) r rs

-------------------------------------------------------------------------------
-- * Parsing
-------------------------------------------------------------------------------
-- | The result of a parse.
data Result s i a
  = Ended i
    -- ^ The parser ended. The 'i' is the rest of the input (which may be
    -- empty) that it didn't manage to parse.
  | Parsed a i (i -> ST s (Result s i a))
    -- ^ The parser parsed something. The 'i' is the rest of the input, and the
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

{-# SPECIALISE parse :: [State s a t a] -> [State s a t a] -> ST s () -> Pos -> [t] -> ST s (Result s [t] a) #-}
-- | The internal parsing routine
parse :: ListLike i t
      => [State s a t a] -- ^ States to process at this position
      -> [State s a t a] -- ^ States to process at the next position
      -> ST s ()         -- ^ Computation that resets the continuation refs of productions
      -> Pos             -- ^ The current position in the input string
      -> i               -- ^ The input string
      -> ST s (Result s i a)
parse []      []    !reset !_   !ts = do
  reset
  return $ Ended ts
parse []      !next !reset !pos !ts = do
  reset
  parse next [] (return ()) (pos + 1) (safeTail ts)
parse (st:ss) !next !reset !pos !ts = case st of
  Final a -> return $ Parsed a ts $ parse ss next reset pos
  State spos pr scont -> case pr of
    Terminal f p -> case uncons ts of
      Just (t, _) | f t -> parse ss (State spos (($ t) <$> p) scont : next) reset pos ts
      _                 -> parse ss next reset pos ts
    NonTerminal r p -> do
      rkref <- readSTRef $ ruleConts r
      ks    <- readSTRef rkref
      writeSTRef rkref (Cont spos p scont : ks)
      nulls' <- nullable r
      let notExpanded = null ks
          p'          = liftYoneda p
          nulls       = fmap (\a -> State spos (lowerYoneda $ ($ a) <$> p') scont) nulls'
      if notExpanded then do
        let prods = fmap (\pr' -> State pos pr' rkref) (ruleProds r)
        parse (nulls ++ prods ++ ss) next ((writeSTRef (ruleConts r) =<< newSTRef mempty) >> reset) pos ts
      else
        parse (nulls ++ ss) next reset pos ts
    Pure a | spos /= pos -> do
      conts <- simplifyCont scont
      parse (map (contToState a) conts ++ ss) next reset pos ts
    _ -> parse ss next reset pos ts

{-# SPECIALISE parser :: (forall r. Grammar r (Prod r t a)) -> [t] -> ST s (Result s [t] a) #-}
-- | Create a parser from the given grammar.
parser :: ListLike i t
       => (forall r. Grammar r (Prod r t a))
       -> i
       -> ST s (Result s i a)
parser g xs = do
  s <- initialState $ grammarM g
  parse [s] [] (return ()) 0 xs

-- | Return all parses from the result of a given parser. The result may
--   contain partial parses and what's left of the input in each of them.
allParses :: (forall s. ST s (Result s i a)) -> [(a, i)]
allParses p = runST $ p >>= go
  where
    go :: Result s i a -> ST s [(a, i)]
    go r = case r of
      Ended _      -> return []
      Parsed a i k -> fmap ((a, i) :) $ go =<< k i

{-# SPECIALISE fullParses :: (forall s. ST s (Result s [t] a)) -> [a] #-}
-- | Return all parses that reached the end of the input from the result of a
--   given parser.
fullParses :: ListLike i t => (forall s. ST s (Result s i a)) -> [a]
fullParses p = runST $ p >>= go
  where
    go :: ListLike i t => Result s i a -> ST s [a]
    go r = case r of
      Ended _ -> return []
      Parsed a i k
        | ListLike.null i -> fmap (a :) $ go =<< k i
        | otherwise       -> go =<< k i
