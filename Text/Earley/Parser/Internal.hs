{-# LANGUAGE CPP, BangPatterns, DeriveFunctor, GADTs, Rank2Types, RecursiveDo, LambdaCase #-}
-- | This module exposes the internals of the package: its API may change
-- independently of the PVP-compliant version number.
module Text.Earley.Parser.Internal where
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
import Data.Semigroup
import Control.Category (Category)
import qualified Control.Category as C
import Data.Traversable (for)
import Data.Functor.Contravariant (contramap)

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
mkRule p = mdo
  c <- newSTRef =<< newSTRef mempty
  computeNullsRef <- newSTRef $ do
    writeSTRef computeNullsRef $ return []
    ns <- unResults $ prodNulls p
    writeSTRef computeNullsRef $ return ns
    return ns
  return $ Rule (removeNulls p) c (Results $ join $ readSTRef computeNullsRef)

prodNulls :: ProdR s r e t a -> Results s a
prodNulls prod = case prod of
  Terminal {}     -> empty
  NonTerminal r p -> ruleNulls r <**> prodNulls p
  Pure a          -> pure a
  Alts as p       -> mconcat (map prodNulls as) <**> prodNulls p
  Many a p        -> prodNulls (pure [] <|> pure <$> a) <**> prodNulls p
  Named p _       -> prodNulls p
  Constraint p _  -> prodNulls p
  Disamb p d      -> Results $ do
    ps <- unResults $ prodNulls p
    ds <- unResults $ prodNulls d
    pure $ ($ ps) =<< ds

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
  Constraint p n   -> Constraint (removeNulls p) n
  Disamb p d       -> Disamb (removeNulls p) d

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

instance Semigroup (Results s a) where
  (<>) = (<|>)

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
        -> !(ResultsCont s a b)
        -> !BirthPos
        -> !(Conts s r e t b c)
        -> State s r e t c
  Final :: !(Results s a) -> State s r e t a

newtype ResultsCont s a b = ResultsCont {unResultsCont :: [a] -> Results s b}
  deriving(Functor)

instance Category (ResultsCont s) where
  id = ResultsCont (Results . pure)
  ResultsCont f . ResultsCont g = ResultsCont $ \xs -> Results $ do
    ys <- unResults (g xs)
    unResults (f ys)

resultArr' :: ([a] -> [b]) -> ResultsCont s a b
resultArr' f = ResultsCont (Results . pure . f)

resultArrs :: [a -> b] -> ResultsCont s a b
resultArrs f = ResultsCont (Results . pure . liftA2 ($) f)

resultArrs' :: [[a] -> [b]] -> ResultsCont s a b
resultArrs' f = ResultsCont (Results . pure . go)
  where go x = ($ x) =<< f

resultBind :: ResultsCont s a b -> Results s a -> Results s b
resultBind (ResultsCont f) (Results x) = Results $ do
  y <- x
  unResults (f y)

manyResults :: [a] -> Results s a
manyResults = Results. pure

-- | A continuation accepting an @a@ and producing a @b@.
data Cont s r e t a b where
  Cont      :: !(ResultsCont s a b)
            -> !(ProdR s r e t ([b] -> [c]))
            -> !(ResultsCont s c d)
            -> !(Conts s r e t d e')
            -> Cont s r e t a e'
  FinalCont :: ResultsCont s a c -> Cont s r e t a c

data Conts s r e t a c = Conts
  { conts     :: !(STRef s [Cont s r e t a c])
  , contsArgs :: !(STRef s (Maybe (STRef s (Results s a))))
  }

newConts :: STRef s [Cont s r e t a c] -> ST s (Conts s r e t a c)
newConts r = Conts r <$> newSTRef Nothing

contraMapCont :: ResultsCont s b a -> Cont s r e t a c -> Cont s r e t b c
contraMapCont f (Cont g p args cs) = Cont (f >>> g) p args cs
contraMapCont f (FinalCont args)   = FinalCont (f >>> args)

contToState :: BirthPos -> Results s a -> Cont s r e t a c -> State s r e t c
contToState pos r (Cont g p args cs) =
  State
    p
    (ResultsCont $ \f -> (args <<< resultArrs' f <<< g) `resultBind` r)
    pos
    cs
contToState _ r (FinalCont args) = Final $ resultBind args r

-- | Strings of non-ambiguous continuations can be optimised by removing
-- indirections.
simplifyCont :: Conts s r e t b a -> ST s [Cont s r e t b a]
simplifyCont Conts {conts = cont} = readSTRef cont >>= go False
  where
    go !_ [Cont g (Pure f) args cont'] = do
      ks' <- simplifyCont cont'
      go True $ map (contraMapCont $ args <<< resultArr' f <<< g) ks'
    go True ks = do
      writeSTRef cont ks
      return ks
    go False ks = return ks

-------------------------------------------------------------------------------
-- * Grammars
-------------------------------------------------------------------------------
-- | Given a grammar, construct an initial state.
initialState :: ProdR s a e t a -> ST s (State s a e t a)
initialState p = State p C.id Previous <$> (newConts =<< newSTRef [FinalCont C.id])

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
      => [State s a e t a]
      -- ^ States to process at this position, S(k) in the nomenclature
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
        (emptyParseEnv $ ListLike.drop 1 $ input env) {curPos = curPos env + 1}
parse [] env = do
  reset env
  return $ Parsed (concat <$> sequence (results env)) (curPos env) (input env)
         $ parse [] env {results = [], reset = return ()}
parse (st:ss) env = case st of
  Final res -> parse ss env {results = unResults res : results env}
  State pr args pos scont -> case pr of
    -- Scanning operation
    Terminal f p -> case ListLike.uncons (input env) >>= f . fst of
      -- We have a state S(k) of the form (X → α • a β, j)
      -- and thus add (X → α a • β, j) to S(k+1)
      -- In our case, advancing the dot past a terminal means applying the
      -- results of that terminal to the input of the continuation
      -- TODO: applying args to a single 'a' here is not correct in the
      -- presence of disambiguation
      Just a -> parse ss env {next = State p (args <<< resultArrs [($ a)]) Previous scont
                                   : next env}
      Nothing -> parse ss env
    -- Prediction operation
    -- For every state in S(k) of the form (X → α • Y β, j)...
    NonTerminal r p -> do
      rkref <- readSTRef $ ruleConts r
      ks    <- readSTRef rkref
      writeSTRef rkref (Cont C.id (fmap fmap p) args scont : ks)
      ns    <- unResults $ ruleNulls r
      -- ...add (Y → • γ, k) to S(k) for every production in the grammar with Y
      -- on the left-hand side (Y → γ).
      let addNullState
            | null ns = id
            | otherwise = (:)
                        $ State p (ResultsCont $ \f -> args `resultBind` manyResults (liftA2 ($) f ns)) pos scont
      if null ks then do -- The rule has not been expanded at this position.
        st' <- State (ruleProd r) C.id Current <$> newConts rkref
        parse (addNullState $ st' : ss)
              env {reset = resetConts r >> reset env}
      else -- The rule has already been expanded at this position.
        parse (addNullState ss) env
    -- Completion operation
    -- For every state in S(k) of the form (Y → γ •, j)...
    Pure a
      -- Skip following continuations that stem from the current position; such
      -- continuations are handled separately.
      | pos == Current -> parse ss env
      | otherwise -> do
        let argsRef = contsArgs scont
        masref  <- readSTRef argsRef
        case masref of
          Just asref -> do -- The continuation has already been followed at this position.
            -- TODO: Applying args to a single a is incorrect in the presence
            -- of disambiguation
            modifySTRef asref $ mappend $ unResultsCont args [a]
            parse ss env
          Nothing    -> do -- It hasn't.
            -- ...find all states in S(j) of the form (X → α • Y β, i) and add
            -- (X → α Y • β, i) to S(k).
            -- In this implementation, advancing the dot requires applying 'a'
            -- here to the continuation.
            -- TODO: Applying args to a single a is incorrect in the presence
            -- of disambiguation
            asref <- newSTRef $ unResultsCont args [a]
            writeSTRef argsRef $ Just asref
            ks  <- simplifyCont scont
            res <- lazyResults $ unResults =<< readSTRef asref
            let kstates = map (contToState pos res) ks
            parse (kstates ++ ss)
                  env {reset = writeSTRef argsRef Nothing >> reset env}
    -- We need to add p with a continuation which takes into account 'd'
    Disamb p (Pure d) -> do
      parse (State p (args <<< resultArr' d) pos scont : ss) env
    Disamb p d -> do
      scont' <- newConts =<< newSTRef [Cont C.id d args scont]
      parse (State p C.id Previous scont' : ss) env
    -- For every alternative, add a state for that production all pointing to
    -- the same continuation.
    Alts as (Pure f) -> do
      -- TODO: is resultArrs safe in the presence of disambiguation
      let args' = args <<< resultArrs [f] 
          sts   = [State a args' pos scont | a <- as]
      parse (sts ++ ss) env
    Alts as p -> do
      scont' <- newConts =<< newSTRef [Cont C.id (fmap fmap p) args scont]
      let sts = [State a C.id Previous scont' | a <- as]
      parse (sts ++ ss) env
    -- Rustle up a left-recursive non-terminal and add it to the states to be
    -- processed next.
    Many p q -> mdo
      r <- mkRule $ pure [] <|> (:) <$> p <*> NonTerminal r (Pure id)
      parse (State (NonTerminal r q) args pos scont : ss) env
    -- Insert a state for the named production, but add the name to the list of
    -- names for this position
    Named pr' n -> parse (State pr' args pos scont : ss)
                         env {names = n : names env}
    -- Insert a state whose continuation filters any results
    Constraint pr' c -> parse (State pr' (args <<< test) pos scont : ss) env
      where test = resultArr' (filter c)

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
