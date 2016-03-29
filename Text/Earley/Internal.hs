{-# LANGUAGE CPP, BangPatterns, DeriveFunctor, GADTs, Rank2Types, RecursiveDo #-}
-- | This module exposes the internals of the package: its API may change
-- independently of the PVP-compliant version number.
module Text.Earley.Internal where
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.ListLike(ListLike)
import qualified Data.ListLike as ListLike
import Data.STRef
import Text.Earley.Grammar
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

-------------------------------------------------------------------------------
-- * Concrete rules and productions
-------------------------------------------------------------------------------
-- | The concrete rule type that the parser uses
data Rule s r e t a = Rule
  { ruleProd  :: ProdR s r e t a
  , ruleConts :: !(STRef s (STRef s [Cont s r e t a r]))
  , ruleNulls :: !(Results s a)
  }

mkRule :: ProdR s r e t a -> ST s (Rule s r e t a)
{-# INLINE mkRule #-}
mkRule p = mdo
  c <- newSTRef =<< newSTRef mempty
  computeNullsRef <- newSTRef $ do
    writeSTRef computeNullsRef $ return []
    ns <- unResults $ prodNulls p
    writeSTRef computeNullsRef $ return ns
    return ns
  return $ Rule (removeNulls p) c (Results $ join $ readSTRef computeNullsRef)

prodNulls :: ProdR s r e t a -> Results s a
{-# INLINE prodNulls #-}
prodNulls prod = case prod of
  Terminal {}     -> empty
  NonTerminal r p -> ruleNulls r <**> prodNulls p
  Pure a          -> pure a
  Alts as p       -> mconcat (map prodNulls as) <**> prodNulls p
  Many a p        -> prodNulls (pure [] <|> pure <$> a) <**> prodNulls p
  Named p _       -> prodNulls p

-- | Remove (some) nulls from a production
removeNulls :: ProdR s r e t a -> ProdR s r e t a
{-# INLINE removeNulls #-}
removeNulls prod = case prod of
  Terminal {}      -> prod
  NonTerminal {}   -> prod
  Pure _           -> empty
  Alts as (Pure f) -> alts (map removeNulls as) $ Pure f
  Alts {}          -> prod
  Many {}          -> prod
  Named p n        -> Named (removeNulls p) n

type ProdR s r e t a = Prod (Rule s r) e t a

resetConts :: Rule s r e t a -> ST s ()
{-# INLINE resetConts #-}
resetConts r = do
  writeSTRef (ruleConts r) =<< newSTRef mempty

-------------------------------------------------------------------------------
-- * Delayed results
-------------------------------------------------------------------------------
newtype Results s a = Results { unResults :: ST s [a] }

instance Functor (Results s) where
  {-# INLINE fmap #-}
  fmap f (Results stas) = Results $ fmap f <$> stas

reverseResults :: Results s a -> Results s a
{-# INLINE reverseResults #-}
reverseResults (Results stas) = Results $ fmap reverse stas

lazyResults :: Results s a -> ST s (Results s a)
{-# INLINE lazyResults #-}
lazyResults (Results stas) = mdo
  resultsRef <- newSTRef $ do
    as <- stas
    writeSTRef resultsRef $ return as
    return as
  return $ Results $ join $ readSTRef resultsRef

instance Applicative (Results s) where
  {-# INLINE pure #-}
  pure = Results . pure . pure
  {-# INLINE (<*>) #-}
  Results sfs <*> Results sxs = Results $ (<*>) <$> sfs <*> sxs

instance Alternative (Results s) where
  {-# INLINE empty #-}
  empty = Results $ pure []
  {-# INLINE (<|>) #-}
  Results sxs <|> Results sys = Results $ (<|>) <$> sxs <*> sys

instance Monoid (Results s a) where
  {-# INLINE mempty #-}
  mempty = empty
  {-# INLINE mappend #-}
  mappend = (<|>)

-------------------------------------------------------------------------------
-- * States and continuations
-------------------------------------------------------------------------------
data BirthPos
  = Previous
  | Current
  deriving Eq

-- | An Earley state with result type @a@.
data State s r e t a where
  State :: !(ProdR s r e t a)
        -> !(Results s (a -> b))
        -> !BirthPos
        -> !(Conts s r e t b c)
        -> State s r e t c
  Final :: !(Results s a) -> State s r e t a

-- | A continuation accepting an @a@ and producing a @b@.
data Cont s r e t a b where
  Cont      :: !(Results s (a -> b))
            -> !(ProdR s r e t (b -> c))
            -> !(Results s (c -> d))
            -> !(Conts s r e t d e')
            -> Cont s r e t a e'
  FinalCont :: !(Results s (a -> c)) -> Cont s r e t a c

data Conts s r e t a c = Conts
  { conts     :: !(STRef s [Cont s r e t a c])
  , contsArgs :: !(STRef s (Maybe (STRef s (Results s a))))
  }

newConts :: STRef s [Cont s r e t a c] -> ST s (Conts s r e t a c)
{-# INLINE newConts #-}
newConts r = Conts r <$> newSTRef Nothing

contraMapCont :: Results s (b -> a) -> Cont s r e t a c -> Cont s r e t b c
{-# INLINE contraMapCont #-}
contraMapCont f (Cont pre p post cs) = Cont (flip (.) <$> f <*> pre) p post cs
contraMapCont f (FinalCont res) = FinalCont (flip (.) <$> f <*> res)

contToState :: BirthPos -> Results s a -> Cont s r e t a c -> State s r e t c
{-# INLINE contToState #-}
contToState pos r (Cont pre p post cs) = State p ((\a f h i -> h $ i $ f a) <$> r <*> pre <*> post) pos cs
contToState _   r (FinalCont res) = Final $ r <**> res

-- | Strings of non-ambiguous continuations can be optimised by removing
-- indirections.
simplifyCont :: Conts s r e t b a -> ST s [Cont s r e t b a]
{-# INLINE simplifyCont #-}
simplifyCont Conts {conts = cont} = readSTRef cont >>= go False
  where
    go !_ [Cont pre (Pure f) post cont'] = do
      ks' <- simplifyCont cont'
      -- res <- lazyResults $ (\a b -> b . f . a) <$> g <*> post
      let res = (\a b -> b . f . a) <$> pre <*> post
      go True $ map (contraMapCont res) ks'
    go True ks = do
      writeSTRef cont ks
      return ks
    go False ks = return ks

-------------------------------------------------------------------------------
-- * Grammars
-------------------------------------------------------------------------------
-- | Given a grammar, construct an initial state.
initialState :: ProdR s a e t a -> ST s (State s a e t a)
{-# INLINE initialState #-}
initialState p = State p (pure id) Previous <$> (newConts =<< newSTRef [FinalCont $ pure id])

-------------------------------------------------------------------------------
-- * Parsing
-------------------------------------------------------------------------------
-- | A parsing report, which contains fields that are useful for presenting
-- errors to the user if a parse is deemed a failure.  Note however that we get
-- a report even when we successfully parse something.
data Report e i = Report
  { position   :: Int -- ^ The final position in the input (0-based) that the
                      -- parser reached.
  , expected   :: [e] -- ^ The named productions processed at the final
                      -- position.
  , unconsumed :: i   -- ^ The part of the input string that was not consumed,
                      -- which may be empty.
  } deriving (Eq, Ord, Read, Show)

-- | The result of a parse.
data Result s e i a
  = Ended (Report e i)
    -- ^ The parser ended.
  | Parsed (ST s [a]) Int i (ST s (Result s e i a))
    -- ^ The parser parsed a number of @a@s.  These are given as a computation,
    -- @'ST' s [a]@ that constructs the 'a's when run.  We can thus save some
    -- work by ignoring this computation if we do not care about the results.
    -- The 'Int' is the position in the input where these results were
    -- obtained, the @i@ the rest of the input, and the last component is the
    -- continuation.
  deriving Functor

safeHead :: ListLike i t => i -> Maybe t
{-# INLINE safeHead #-}
safeHead ts
  | ListLike.null ts = Nothing
  | otherwise        = Just $ ListLike.head ts

data ParseEnv s e i t a = ParseEnv
  { results :: ![ST s [a]]
    -- ^ Results ready to be reported (when this position has been processed)
  , next    :: ![State s a e t a]
    -- ^ States to process at the next position
  , reset   :: !(ST s ())
    -- ^ Computation that resets the continuation refs of productions
  , names   :: ![e]
    -- ^ Named productions encountered at this position
  , curPos  :: !Int
    -- ^ The current position in the input string
  , input   :: !i
    -- ^ The input string
  }

emptyParseEnv :: i -> ParseEnv s e i t a
{-# INLINE emptyParseEnv #-}
emptyParseEnv i = ParseEnv
  { results = mempty
  , next    = mempty
  , reset   = return ()
  , names   = mempty
  , curPos  = 0
  , input   = i
  }

{-# SPECIALISE parse :: [State s a e t a]
                     -> ParseEnv s e [t] t a
                     -> ST s (Result s e [t] a) #-}
-- | The internal parsing routine
parse :: ListLike i t
      => [State s a e t a] -- ^ States to process at this position
      -> ParseEnv s e i t a
      -> ST s (Result s e i a)
parse [] env@ParseEnv {results = [], next = []} = do
  reset env
  return $ Ended Report
    { position   = curPos env
    , expected   = names env
    , unconsumed = input env
    }
parse [] env@ParseEnv {results = []} = do
  reset env
  parse (reverse $ next env)
        (emptyParseEnv $ ListLike.tail $ input env) {curPos = curPos env + 1}
parse [] env = do
  reset env
  return $ Parsed (concat <$> sequence (results env)) (curPos env) (input env)
         $ parse [] env {reset = return (), results = []}
parse (st:ss) env = case st of
  Final res -> parse ss env {results = unResults res : results env}
  State pr res pos scont -> case pr of
    Terminal f p -> case safeHead (input env) >>= f of
      Just a -> parse ss env {next = State p ((\g h -> g $ h a) <$> res) Previous scont
                                   : next env}
      Nothing -> parse ss env
    NonTerminal r p -> do
      rkref <- readSTRef $ ruleConts r
      ks    <- readSTRef rkref
      writeSTRef rkref (Cont (pure id) p res scont : ks)
      ns    <- unResults $ ruleNulls r
      let addNullState
            | null ns = id
            | otherwise = (:)
                        $ State p ((\f a h -> f (h a)) <$> res <*> Results (pure ns)) pos scont
      if null ks then do -- The rule has not been expanded at this position.
        st' <- State (ruleProd r) (pure id) Current <$> newConts rkref
        parse (addNullState $ st' : ss)
              env {reset = modifySTRef rkref reverse >> resetConts r >> reset env}
      else -- The rule has already been expanded at this position.
        parse (addNullState ss) env
    Pure a
      -- Skip following continuations that stem from the current position; such
      -- continuations are handled separately.
      | pos == Current -> parse ss env
      | otherwise -> do
        let argsRef = contsArgs scont
        masref  <- readSTRef argsRef
        let res' = ($ a) <$> res
        case masref of
          Just asref -> do -- The continuation has already been followed at this position.
            modifySTRef asref $ mappend res'
            parse ss env
          Nothing    -> do -- It hasn't.
            res'Ref <- newSTRef res'
            writeSTRef argsRef $ Just res'Ref
            ks  <- simplifyCont scont
            -- finalResults <- lazyResults $ Results $ join $ unResults <$> readSTRef res'Ref
            let finalResults = Results $ join $ unResults <$> readSTRef res'Ref
            parse (map (contToState pos finalResults) ks ++ ss)
                  env {reset = modifySTRef res'Ref reverseResults >> writeSTRef argsRef Nothing >> reset env}
    Alts as (Pure f) -> do
      -- res' <- lazyResults $ (. f) <$> res
      let res' = (. f) <$> res
      parse ([State a res' pos scont | a <- as] ++ ss) env
    Alts as p -> do
      scont' <- newConts =<< newSTRef [Cont (pure id) p res scont]
      parse ([State a (pure id) Previous scont' | a <- as] ++ ss) env
    Many p q -> mdo
      r <- mkRule $ pure [] <|> (:) <$> p <*> NonTerminal r (Pure id)
      parse (State (NonTerminal r q) res pos scont : ss) env
    Named pr' n -> parse (State pr' res pos scont : ss)
                         env {names = n : names env}

-- | Create a parser from the given grammar.
parser :: ListLike i t
       => (forall r. Grammar r (Prod r e t a))
       -> ST s (i -> ST s (Result s e i a))
{-# INLINE parser #-}
parser g = do
  let nt x = NonTerminal x $ pure id
  s <- initialState =<< runGrammar (fmap nt . mkRule) g
  return $ parse [s] . emptyParseEnv

-- | Return all parses from the result of a given parser. The result may
-- contain partial parses. The 'Int's are the position at which a result was
-- produced.
allParses :: (forall s. ST s (i -> ST s (Result s e i a)))
          -> i
          -> ([(a, Int)], Report e i)
{-# INLINE allParses #-}
allParses p i = runST $ p >>= ($ i) >>= go
  where
    go :: Result s e i a -> ST s ([(a, Int)], Report e i)
    go r = case r of
      Ended rep           -> return ([], rep)
      Parsed mas cpos _ k -> do
        as <- mas
        fmap (first (zip as (repeat cpos) ++)) $ go =<< k

-- | Return all parses that reached the end of the input from the result of a
--   given parser.
fullParses :: ListLike i t
           => (forall s. ST s (i -> ST s (Result s e i a)))
           -> i
           -> ([a], Report e i)
{-# INLINE fullParses #-}
fullParses p i = runST $ p >>= ($ i) >>= go
  where
    go :: ListLike i t => Result s e i a -> ST s ([a], Report e i)
    go r = case r of
      Ended rep -> return ([], rep)
      Parsed mas _ i' k
        | ListLike.null i' -> do
          as <- mas
          fmap (first (as ++)) $ go =<< k
        | otherwise -> go =<< k

-- | See e.g. how far the parser is able to parse the input string before it
-- fails.  This can be much faster than getting the parse results for highly
-- ambiguous grammars.
report :: ListLike i t
       => (forall s. ST s (i -> ST s (Result s e i a)))
       -> i
       -> Report e i
{-# INLINE report #-}
report p i = runST $ p >>= ($ i) >>= go
  where
    go :: ListLike i t => Result s e i a -> ST s (Report e i)
    go r = case r of
      Ended rep      -> return rep
      Parsed _ _ _ k -> go =<< k
