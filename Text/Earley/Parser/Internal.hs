{-# LANGUAGE CPP, BangPatterns, DeriveFunctor, GADTs, Rank2Types, RecursiveDo #-}
-- | This module exposes the internals of the package: its API may change
-- independently of the PVP-compliant version number.
module Text.Earley.Parser.Internal where
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.ST.Lazy
import Data.ListLike(ListLike)
import qualified Data.ListLike as ListLike
import Data.STRef.Lazy
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
  , ruleNulls :: !(ST s ([a], Bool))
  -- ^ Null productions. The bool specifies whether there are any null
  -- productions and can be inspected without forcing the list of results
  }

mkRule :: ProdR s r e t a -> ST s (Rule s r e t a)
mkRule p = mdo
  c <- newSTRef =<< newSTRef mempty
  computeNullsRef <- newSTRef $ do
    writeSTRef computeNullsRef $ return ([], False)
    nullResults <- prodNulls True p
    if null nullResults then
      return ([], False)
    else mdo
      writeSTRef computeNullsRef $ return (result, True)
      result <- prodNulls False p
      return (result, True)
  return $ Rule (removeNulls p) c $ join $ readSTRef computeNullsRef

prodNulls :: Bool -> ProdR s r e t a -> ST s [a]
prodNulls strict prod = case prod of
  Terminal {} -> return mempty
  NonTerminal r p -> do
    (ns, hasNulls) <- ruleNulls r
    if strict && not hasNulls then
      return []
    else do
      pres <- prodNulls strict p
      return $ ns <**> pres
  Pure a -> return $ pure a
  Alts as p -> do
    asres <- concat <$> mapM (prodNulls strict) as
    if strict && null asres then
      return []
    else do
      pres <- prodNulls strict p
      return $ asres <**> pres
  Many a p -> mdo
    r <- mkRule $ pure [] <|> (:) <$> a <*> NonTerminal r (Pure id)
    prodNulls strict $ NonTerminal r p
  Named p _ -> prodNulls strict p

-- | Remove (some) nulls from a production
removeNulls :: ProdR s r e t a -> ProdR s r e t a
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
resetConts r = writeSTRef (ruleConts r) =<< newSTRef mempty

-------------------------------------------------------------------------------
-- * Delayed results
-------------------------------------------------------------------------------
newtype Results s a = Results { unResults :: ST s [a] }
  deriving Functor

lazyResults :: ST s [a] -> ST s (Results s a)
lazyResults stas = mdo
  resultsRef <- newSTRef $ do
    as <- stas
    writeSTRef resultsRef $ return as
    return as
  return $ Results $ join $ readSTRef resultsRef

instance Applicative (Results s) where
  pure  = return
  (<*>) = ap

instance Alternative (Results s) where
  empty = Results $ pure []
  Results sxs <|> Results sys = Results $ (<|>) <$> sxs <*> sys

instance Monad (Results s) where
  return = Results . pure . pure
  Results stxs >>= f = Results $ do
    xs <- stxs
    concat <$> mapM (unResults . f) xs

instance Monoid (Results s a) where
  mempty = empty
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
        -> !(a -> Results s b)
        -> !BirthPos
        -> !(Conts s r e t b c)
        -> State s r e t c
  Final :: !(Results s a) -> State s r e t a

-- | A continuation accepting an @a@ and producing a @b@.
data Cont s r e t a b where
  Cont      :: !(a -> Results s b)
            -> !(ProdR s r e t (b -> c))
            -> !(c -> Results s d)
            -> !(Conts s r e t d e')
            -> Cont s r e t a e'
  FinalCont :: (a -> Results s c) -> Cont s r e t a c

data Conts s r e t a c = Conts
  { conts     :: !(STRef s [Cont s r e t a c])
  , contsArgs :: !(STRef s (Maybe (STRef s (Results s a))))
  }

newConts :: STRef s [Cont s r e t a c] -> ST s (Conts s r e t a c)
newConts r = Conts r <$> newSTRef Nothing

contraMapCont :: (b -> Results s a) -> Cont s r e t a c -> Cont s r e t b c
contraMapCont f (Cont g p args cs) = Cont (f >=> g) p args cs
contraMapCont f (FinalCont args)   = FinalCont (f >=> args)

contToState :: BirthPos -> Results s a -> Cont s r e t a c -> State s r e t c
contToState pos r (Cont g p args cs) = State p (\f -> fmap f (r >>= g) >>= args) pos cs
contToState _   r (FinalCont args)   = Final $ r >>= args

-- | Strings of non-ambiguous continuations can be optimised by removing
-- indirections.
simplifyCont :: Conts s r e t b a -> ST s [Cont s r e t b a]
simplifyCont Conts {conts = cont} = readSTRef cont >>= go False
  where
    go !_ [Cont g (Pure f) args cont'] = do
      ks' <- simplifyCont cont'
      go True $ map (contraMapCont $ \b -> fmap f (g b) >>= args) ks'
    go True ks = do
      writeSTRef cont ks
      return ks
    go False ks = return ks

-------------------------------------------------------------------------------
-- * Grammars
-------------------------------------------------------------------------------
-- | Given a grammar, construct an initial state.
initialState :: ProdR s a e t a -> ST s (State s a e t a)
initialState p = State p pure Previous <$> (newConts =<< newSTRef [FinalCont pure])

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

{-# INLINE safeHead #-}
safeHead :: ListLike i t => i -> Maybe t
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

{-# INLINE emptyParseEnv #-}
emptyParseEnv :: i -> ParseEnv s e i t a
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
  parse (next env)
        (emptyParseEnv $ ListLike.tail $ input env) {curPos = curPos env + 1}
parse [] env = do
  reset env
  return $ Parsed (concat <$> sequence (results env)) (curPos env) (input env)
         $ parse [] env {results = [], reset = return ()}
parse (st:ss) env = case st of
  Final res -> parse ss env {results = unResults res : results env}
  State pr args pos scont -> case pr of
    Terminal f p -> case safeHead (input env) >>= f of
      Just a -> parse ss env {next = State p (args . ($ a)) Previous scont
                                   : next env}
      Nothing -> parse ss env
    NonTerminal r p -> do
      rkref <- readSTRef $ ruleConts r
      ks    <- readSTRef rkref
      writeSTRef rkref (Cont pure p args scont : ks)
      (ns, hasNulls) <- ruleNulls r
      let addNullState
            | hasNulls = (:)
                        $ State p (\f -> Results (pure $ map f ns) >>= args) pos scont
            | otherwise = id
      if null ks then do -- The rule has not been expanded at this position.
        st' <- State (ruleProd r) pure Current <$> newConts rkref
        parse (addNullState $ st' : ss)
              env {reset = resetConts r >> reset env}
      else -- The rule has already been expanded at this position.
        parse (addNullState ss) env
    Pure a
      -- Skip following continuations that stem from the current position; such
      -- continuations are handled separately.
      | pos == Current -> parse ss env
      | otherwise -> do
        let argsRef = contsArgs scont
        masref  <- readSTRef argsRef
        case masref of
          Just asref -> do -- The continuation has already been followed at this position.
            modifySTRef asref $ mappend $ args a
            parse ss env
          Nothing    -> do -- It hasn't.
            asref <- newSTRef $ args a
            writeSTRef argsRef $ Just asref
            ks  <- simplifyCont scont
            res <- lazyResults $ join $ unResults <$> readSTRef asref
            let kstates = map (contToState pos res) ks
            parse (kstates ++ ss)
                  env {reset = writeSTRef argsRef Nothing >> reset env}
    Alts as (Pure f) -> do
      let args' = args . f
          sts   = [State a args' pos scont | a <- as]
      parse (sts ++ ss) env
    Alts as p -> do
      scont' <- newConts =<< newSTRef [Cont pure p args scont]
      let sts = [State a pure Previous scont' | a <- as]
      parse (sts ++ ss) env
    Many p q -> mdo
      r <- mkRule $ pure [] <|> (:) <$> p <*> NonTerminal r (Pure id)
      parse (State (NonTerminal r q) args pos scont : ss) env
    Named pr' n -> parse (State pr' args pos scont : ss)
                         env {names = n : names env}

type Parser e i a = forall s. i -> ST s (Result s e i a)

{-# INLINE parser #-}
-- | Create a parser from the given grammar.
parser
  :: ListLike i t
  => (forall r. Grammar r (Prod r e t a))
  -> Parser e i a
parser g i = do
  let nt x = NonTerminal x $ pure id
  s <- initialState =<< runGrammar (fmap nt . mkRule) g
  parse [s] $ emptyParseEnv i

-- | Return all parses from the result of a given parser. The result may
-- contain partial parses. The 'Int's are the position at which a result was
-- produced.
--
-- The elements of the returned list of results are sorted by their position in
-- ascending order.  If there are multiple results at the same position they
-- are returned in an unspecified order.
allParses
  :: Parser e i a
  -> i
  -> ([(a, Int)], Report e i)
allParses p i = runST $ p i >>= go
  where
    go :: Result s e i a -> ST s ([(a, Int)], Report e i)
    go r = case r of
      Ended rep           -> return ([], rep)
      Parsed mas cpos _ k -> do
        as <- mas
        fmap (first (zip as (repeat cpos) ++)) $ go =<< k

{-# INLINE fullParses #-}
-- | Return all parses that reached the end of the input from the result of a
-- given parser.
--
-- If there are multiple results they are returned in an unspecified order.
fullParses
  :: ListLike i t
  => Parser e i a
  -> i
  -> ([a], Report e i)
fullParses p i = runST $ p i >>= go
  where
    go :: ListLike i t => Result s e i a -> ST s ([a], Report e i)
    go r = case r of
      Ended rep -> return ([], rep)
      Parsed mas _ i' k
        | ListLike.null i' -> do
          as <- mas
          fmap (first (as ++)) $ go =<< k
        | otherwise -> go =<< k

{-# INLINE report #-}
-- | See e.g. how far the parser is able to parse the input string before it
-- fails.  This can be much faster than getting the parse results for highly
-- ambiguous grammars.
report
  :: Parser e i a
  -> i
  -> Report e i
report p i = runST $ p i >>= go
  where
    go :: Result s e i a -> ST s (Report e i)
    go r = case r of
      Ended rep      -> return rep
      Parsed _ _ _ k -> go =<< k
